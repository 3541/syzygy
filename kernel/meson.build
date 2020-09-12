# Declarations

kernel_src = run_command(find, [
                          meson.current_source_dir() + '/src',
                          '-type', 'f',
                          '-name',
                          '*.rs'
                        ],
                         check: true).stdout().strip().split('\n')

kernel_asm_src = files([
                        'src/boot/arch/' + cpu + '/boot.asm',
                        'src/boot/arch/' + cpu + '/check.asm',
                        'src/boot/arch/' + cpu + '/multiboot_header.asm',
                        ])

kernel_asm_obj = nasm_gen.process(kernel_asm_src)

rustc_flags += '-Cforce-frame-pointers=yes'

kernel_features = []
if get_option('kernel_log_vga')
  kernel_features += 'log_vga'
endif

if kernel_features.length() > 0
  kernel_features = ['--features', kernel_features]
endif

# Targets

rustc_flags_env = ''
foreach flag : rustc_flags
  rustc_flags_env += flag + ' '
endforeach

kernel_rs = custom_target(
  'libsyzygy_kernel',
  output: 'libsyzygy_kernel.a',
  command: [
    env,
    'RUST_TARGET_PATH=' + meson.source_root() + '/targets',
    'RUSTFLAGS=' + rustc_flags_env,
    run_cargo,
    '@OUTDIR@',
    'syzygy_kernel',
    'libsyzygy_kernel.a',
    cpu + '-pc-elf-none',
    '@OUTPUT@',
    '-Zbuild-std=core,alloc,compiler_builtins',
    '-Zpackage-features',
    kernel_features
  ],
  depend_files: kernel_src,
  console: true
)

kernel = executable(
  'sz_kernel.elf',
  kernel_asm_obj,
  link_with: kernel_rs,
  link_args: [
    '-nostdlib',
    '-ffreestanding',
    '-no-pie',
    '-Wl,-n',
    '-Wl,--gc-sections',
    '-T', meson.current_source_dir() + '/link/' + cpu + '.ld'
  ]
)
