# Variable declarations

cpu = host_machine.cpu_family()
rustc_flags += '-Cforce-frame-pointers=yes'

kernel_src = run_command('find',
                         [
                          meson.source_root() + '/kernel/src',
                          '-name',
                          '*.rs'
                         ],
                        check: true).stdout().strip().split('\n')
kernel_asm_src = files([
                        'src/arch/' + cpu + '/boot.asm',
                        'src/arch/' + cpu + '/multiboot_header.asm'
                      ])
any_features = false
kernel_features = []
if get_option('vga_log')
  kernel_features += 'vga_log'
  any_features = true
endif

if any_features
  kernel_features = ['--features', kernel_features]
endif

# Programs

nasm = find_program('nasm')
asm_gen = generator(
  nasm,
  output: '@BASENAME@.o',
  arguments: [
    '-felf64',
    '@INPUT@',
    '-o', '@OUTPUT@'
  ]
)

xargo = find_program('xargo', native: true)
env = find_program('env', native: true)
nm = find_program('nm', native: true)

# Targets

kernel_asm_objects = asm_gen.process(kernel_asm_src)

rustc_flags_env = ''
foreach flag : rustc_flags
  rustc_flags_env += flag + ' '
endforeach

kernel_lib = custom_target(
  'libsyzygy',
  output: 'libsyzygy.a',
  command: [
    env,
    'RUST_TARGET_PATH=' + meson.source_root() + '/targets',
    'RUSTFLAGS=' + rustc_flags_env,
    'DIST_VERSION=' + meson.project_version(),
    cargo_wrapper,
    'xargo',
    '@OUTDIR@',
    '@OUTDIR@/x86_64-pc-elf-none',
    'libsyzygy.a',
    '@OUTPUT@',
    '--target',
    cpu + '-pc-elf-none',
#    xargo_flags,
    '-p',
    'syzygy',
    '-Zpackage-features',
    kernel_features
  ],
  depend_files: kernel_src,
  console: true
)

kernel = executable(
  'kernel.elf',
  kernel_asm_objects,
  link_with: kernel_lib,
  link_args: [
    '-nostdlib',
    '-ffreestanding',
    '-no-pie',
    '-Wl,-n',
    '-Wl,--gc-sections',
    '-T', meson.current_source_dir() + '/src/arch/' + cpu + '/linker.ld'
  ]
)

kernel_sym = custom_target(
  'kernel-symbols',
  output: 'kernel.sym',
  command: [
    gen_sym,
    kernel
  ],
  capture: true
)
