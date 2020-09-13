project('syzygy', ['rust', 'c'], version: '0.1.0')

# Declarations

cpu = host_machine.cpu_family()

rustc_flags = ['-Zsymbol-mangling-version=v0']

qemu_memory = get_option('vm_mem')
qemu_flags = [
  '-m', qemu_memory.to_string() + 'G',
  '-d', 'cpu_reset',
  '-debugcon', 'stdio'
]

if get_option('kvm')
  qemu_flags += [
    '-enable-kvm'
  ]
endif

if not get_option('vm_display') and not get_option('kernel_log_vga')
  qemu_flags += [ '-display', 'none' ]
endif

# Programs

nasm = find_program('nasm')
nasm_gen = generator(
  nasm,
  output: '@BASENAME@.o',
  arguments: [
    '-felf64',
    '@INPUT@',
    '-o', '@OUTPUT@'
  ]
)
qemu = find_program(
  'qemu-system-' + cpu,
  required: false,
  native: true
)
sudo = find_program('sudo', required: false, native: true)
if not sudo.found()
  sudo = find_program('doas', native: true)
endif
env = find_program('env', native: true)
find = find_program('find', native: true)

cp_stamp = find_program('scripts/cp_stamp.sh', native: true)
make_fs = find_program('scripts/make_fs.sh', native: true)
run_cargo = find_program('scripts/run_cargo.sh', native: true)

# Subdirectories

subdir('kernel')

# Targets

image_dir = meson.current_build_dir() + '/image'
image_files = custom_target(
  'copy-image-files',
  output: 'image_files.stamp',
  command: [
    cp_stamp,
    '@OUTPUT@',
    kernel,
    image_dir + '/boot/'
  ]
)

grub_cfg_file = meson.source_root() + '/boot/grub.cfg'
grub_cfg = custom_target(
  'copy-grub-cfg',
  output: 'grub_cfg.stamp',
  command: [
    cp_stamp,
    '@OUTPUT@',
    grub_cfg_file,
    image_dir + '/boot/grub/grub.cfg'
  ],
  depend_files: grub_cfg_file
)

image = custom_target(
  'make-image',
  build_by_default: true,
  output: 'syzygy.img',
  command: [
    sudo, make_fs,
    image_dir,
    '@OUTPUT@'
  ],
  console: true,
  depends: [grub_cfg, image_files]
)

if qemu.found()
  run_target(
    'run',
    command: [
      qemu,
      '-drive', 'format=raw,file=' + image.full_path(),
      qemu_flags
    ],
    depends: image
  )
endif
