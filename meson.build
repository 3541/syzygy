project('syzygy', ['rust', 'c'], version: '0.1.0')

# Variable declarations
rustc_flags = ['-Zsymbol-mangling-version=v0']
qemu_memory = '4G'
qemu_flags = [
  '-s',
#  '-serial', 'mon:stdio',
  '-m', qemu_memory,
  '-device', 'isa-debug-exit,iobase=0xF4,iosize=0x04',
  '-d', 'cpu_reset',
  '-debugcon', 'stdio'
]
if get_option('kvm')
  qemu_flags += [
    '-enable-kvm',
    '-cpu', 'kvm64,+pdpe1gb,+tsc-deadline,+invtsc'
  ]
else
  qemu_flags += [ '-cpu', 'qemu64,+pdpe1gb' ]
endif
if not get_option('display')
  qemu_flags += [ '-display', 'none' ]
endif

# Programs
qemu = find_program(
  'qemu-system-x86_64',
  required: false,
  native: true
)
bochs = find_program(
  'bochs',
  required: false,
  native: true
)
run_bochs = find_program('scripts/run_bochs.sh', native: true)
grub_mkrescue = find_program('grub-mkrescue', required: false, native: true)
xorriso = find_program('xorriso', required: false, native: true) # This is required for grub-mkrescue
cp_stamp = find_program('scripts/copy_with_stamp.sh', native: true)
mkdir = find_program('mkdir', native: true)
cargo = find_program('cargo', native: true)
cargo_wrapper = find_program('scripts/run_cargo.sh', native: true)
gen_sym = find_program('scripts/sym.sh', native: true)
make_fs = find_program('scripts/make_fs.sh', native: true)
rustfilt = find_program('rustfilt', required: false, native: true)

if not rustfilt.found()
  run_command(cargo,
              [
                'install',
                'rustfilt'
              ],
              check: true)
  rustfilt = find_program('rustfilt', native: true)
endif

# Subdirectories
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

fs_dir = custom_target(
  'make-fs-dir',
  output: 'fs_build',
  command: [mkdir, '-p', '@OUTPUT@' + '/boot/grub'],
)

fs_files = custom_target(
  'copy-fs-files',
  output: 'fs_files.stamp',
  command: [
    cp_stamp,
    '@OUTPUT@',
    kernel,
    initramfs,
    fs_dir.full_path() + '/boot/'
  ],
)

grub_cfg = custom_target(
  'copy-grub-cfg',
  output: 'grub_cfg.stamp',
  command: [
    cp_stamp,
    '@OUTPUT@',
    meson.source_root() + '/kernel/src/arch/x86_common/grub.cfg',
    fs_dir.full_path() + '/boot/grub/grub.cfg'
  ],
  depends: fs_dir,
  depend_files: meson.source_root() + '/kernel/src/arch/x86_common/grub.cfg'
)

if grub_mkrescue.found() and xorriso.found()
  iso = custom_target(
    'make-iso',
    output: 'syzygy.iso',
    command: [
      grub_mkrescue,
      '-o', '@OUTPUT@',
      fs_dir
    ],
    console: true,
    depends: [grub_cfg, fs_files]
  )
endif

image = custom_target(
  'make-image',
  build_by_default: true,
  output: 'syzygy.img',
  command: [
    'sudo', make_fs,
    fs_dir,
    '@OUTPUT@'
  ],
  console: true,
  depends: [grub_cfg, fs_files]
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

  run_target(
    'debug',
    command: [
      qemu,
      '-drive', 'format=raw,file=' + image.full_path(),
      qemu_flags,
      '-S'
    ],
    depends: image
  )
endif

if bochs.found() and not qemu.found()
  run_target(
    'run',
    command: [ run_bochs, bochs.path() ],
    depends: image
  )
endif

if bochs.found()
  run_target(
    'bochs',
    command: [ run_bochs, bochs.path() ],
    depends: image
  )
endif
