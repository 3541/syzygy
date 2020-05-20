project('syzygy', ['rust', 'c'])

# Variable declarations
rustc_flags = ['-Zsymbol-mangling-version=v0']
qemu_memory = '4G'

# Programs
qemu = find_program(
  'qemu-system-x86_64',
  required: false,
  native: true
)
grub_mkrescue = find_program('grub-mkrescue', native: true)
cp_stamp = find_program('./scripts/copy_with_stamp.sh')
mkdir = find_program('mkdir', native: true)
cargo_wrapper = find_program('./scripts/run_cargo.sh')

# Subprojects
subdir('kernel')
subdir('initramfs')

# Targets
initramfs_dir = custom_target(
  'make-initramfs-dir',
  output: 'initramfs_build',
  command: [mkdir, '-p', '@OUTPUT@']
)

initramfs_files = custom_target(
  'copy-initramfs-files',
  # This is a hack
  output: 'initramfs_files.stamp',
  command: [
    cp_stamp,
    '@OUTPUT@',
    kernel_sym,
    initramfs_dir
  ]
)

initramfs = custom_target(
  'make-initramfs',
  output: 'initramfs.fs',
  command: [
    mkinitramfs,
    initramfs_dir,
    '@OUTPUT@'
  ],
  depends: initramfs_files
)

iso_dir = custom_target(
  'make-iso-dir',
  output: 'iso_build',
  command: [mkdir, '-p', '@OUTPUT@' + '/boot/grub'],
)

iso_files = custom_target(
  'copy-iso-files',
  output: 'iso_files.stamp',
  command: [
    cp_stamp,
    '@OUTPUT@',
    kernel,
    initramfs,
    iso_dir.full_path() + '/boot/'
  ],
)

grub_cfg = custom_target(
  'copy-grub-cfg',
  output: 'grub_cfg.stamp',
  command: [
    cp_stamp,
    '@OUTPUT@',
    meson.source_root() + '/kernel/src/arch/x86_common/grub.cfg',
    iso_dir.full_path() + '/boot/grub/grub.cfg'
  ],
  depends: iso_dir,
  depend_files: meson.source_root() + '/kernel/src/arch/x86_common/grub.cfg'
)

iso = custom_target(
  'generate-iso',
  build_by_default: true,
  output: 'syzygy.iso',
  command: [
    grub_mkrescue,
    '-o', '@OUTPUT@',
    iso_dir
  ],
  depends: [grub_cfg, iso_files]
)

if qemu.found()
  run_target(
    'run',
    command: [
      qemu,
      '-cdrom', iso,
      '-s',
      '-serial', 'mon:stdio',
      '-m', qemu_memory,
      '-device', 'isa-debug-exit,iobase=0xF4,iosize=0x04'
    ]
  )
endif
