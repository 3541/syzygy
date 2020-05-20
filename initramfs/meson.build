# Variable declarations

initramfs_src = files([
                       'src/lib.rs',
                       'src/bin/mk.rs'
                     ])

# ree @ meson developers being annoying
#mkinitramfs = executable(
#  'mk',
#  initramfs_src
#)

mkinitramfs = custom_target(
  'mkinitramfs',
  output: 'mk',
  command: [
    cargo_wrapper,
    'cargo',
    '@OUTDIR@',
    '@OUTDIR@',
    'mk',
    '@OUTPUT@',
    '-p', 'initramfs',
    '--bin', 'mk'
  ],
  depend_files: initramfs_src,
  console: true
)
