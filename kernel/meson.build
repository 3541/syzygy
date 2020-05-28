# Variable declarations

cpu = host_machine.cpu_family()
rustc_flags += '-Cforce-frame-pointers=yes'

kernel_src = files([
                    'src/lib.rs',
                    'src/constants.rs',
                    'src/log.rs',
                    'src/panic.rs',
                    'src/sym.rs',
                    'src/sync.rs',
                    'src/vga_text.rs',
                    'src/arch/mod.rs',
                    'src/arch/x86_64/mod.rs',
                    'src/arch/x86_64/port.rs',
                    'src/arch/x86_64/process.rs',
                    'src/arch/x86_64/register.rs',
                    'src/arch/x86_64/driver/mod.rs',
                    'src/arch/x86_64/driver/serial.rs',
                    'src/arch/x86_64/interrupt/mod.rs',
                    'src/arch/x86_64/interrupt/exception.rs',
                    'src/arch/x86_64/interrupt/idt.rs',
                    'src/arch/x86_64/interrupt/pic.rs',
                    'src/driver/mod.rs',
                    'src/driver/acpi/mod.rs',
                    'src/memory/mod.rs',
                    'src/memory/heap.rs',
                    'src/memory/alloc/mod.rs',
                    'src/memory/alloc/bump_allocator.rs',
                    'src/memory/alloc/fake_allocator.rs',
                    'src/memory/frame_allocator/mod.rs',
                    'src/memory/frame_allocator/bitmap_frame_allocator.rs',
                    'src/memory/frame_allocator/watermark_frame_allocator.rs',
                    'src/memory/paging/mod.rs',
                    'src/memory/paging/mapper.rs',
                    'src/memory/paging/table.rs',
                    'src/memory/paging/temp_page.rs',
                    'src/tree/mod.rs'
                  ])
kernel_asm_src = files([
                        'src/arch/' + cpu + '/boot.asm',
                        'src/arch/' + cpu + '/multiboot_header.asm'
                      ])

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
    cargo_wrapper,
    'xargo',
    '@OUTDIR@',
    '@OUTDIR@/x86_64-elf',
    'libsyzygy.a',
    '@OUTPUT@',
    '--target',
    cpu + '-elf',
#    xargo_flags,
    '-p',
    'syzygy'
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
    nm,
    '--numeric-sort',
    '--reverse-sort',
    kernel
  ],
  capture: true
)
