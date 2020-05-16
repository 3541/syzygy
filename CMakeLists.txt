cmake_minimum_required(VERSION 3.16)

project(syzygy
  VERSION 0.0.1
  LANGUAGES C ASM_NASM)

# Don't try to link with nasm
set(CMAKE_ASM_NASM_LINK_EXECUTABLE "ld <CMAKE_ASM_NASM_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")

set(TARGET "x86_64")

set(ISO "${CMAKE_BINARY_DIR}/syzygy.iso")
set(ISO_FILES "${CMAKE_BINARY_DIR}/isofiles")

set(INITRAMFS "${ISO_FILES}/boot/initramfs.fs")
set(INITRAMFS_FILES "${CMAKE_BINARY_DIR}/initramfs_build")

set(QEMU_MEMORY "4G")
# Yes, this should be unquoted.
set(QEMU_FLAGS -s -serial mon:stdio -m "${QEMU_MEMORY}" -device isa-debug-exit,iobase=0xF4,iosize=0x04)

# Yes, this should be quoted.
set(RUSTC_FLAGS "-Zsymbol-mangling-version=v0 -Cforce-frame-pointers=yes")

find_program(GRUB_MKRESCUE grub-mkrescue)
if(NOT GRUB_MKRESCUE)
  message(FATAL_ERROR "Cannot find grub-mkrescue")
endif()

# Xorriso is neccessary but not used
find_program(XORRISO xorriso)
if(NOT XORRISO)
  message(FATAL_ERROR "Cannot find xorriso (required by grub-mkrescue)")
endif()

find_program(QEMU qemu-system-x86_64)
if(NOT QEMU)
  message(WARNING "Cannot find qemu. System can build but not run.")
endif()

find_program(NM nm)
if(NOT NM)
  message(FATAL_ERROR "Cannot find nm.")
endif()

add_subdirectory("kernel")
add_subdirectory("initramfs")

add_custom_target(
  run
  COMMAND ${QEMU} -cdrom ${ISO} ${QEMU_FLAGS}
  DEPENDS iso
  USES_TERMINAL
)

add_custom_target(
  iso ALL
  DEPENDS "${ISO}"
)

add_custom_command(
  OUTPUT "${ISO}"
  COMMAND ${GRUB_MKRESCUE} -o "${ISO}" "${ISO_FILES}"
  DEPENDS copy_iso_files
  USES_TERMINAL
)

add_custom_target(
  copy_iso_files
  DEPENDS "${ISO_FILES}/boot/kernel.bin" "${ISO_FILES}/boot/grub/grub.cfg" "${INITRAMFS}"
)

add_custom_command(
  OUTPUT "${ISO_FILES}/boot/kernel.bin"
  COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:kernel> ${ISO_FILES}/boot/kernel.bin
  DEPENDS kernel
)

add_custom_command(
  OUTPUT "${ISO_FILES}/boot/grub/grub.cfg"
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/kernel/src/arch/x86_common/grub.cfg ${ISO_FILES}/boot/grub/grub.cfg
)

add_custom_command(
  OUTPUT "${INITRAMFS}"
  COMMAND ${MKINITRAMFS} "${INITRAMFS_FILES}" "${INITRAMFS}"
  DEPENDS copy_initramfs_files mkinitramfs
)

add_custom_target(
  copy_initramfs_files
  DEPENDS "${INITRAMFS_FILES}/kernel.sym"
)

add_custom_command(
  OUTPUT "${INITRAMFS_FILES}/kernel.sym"
  COMMAND ${CMAKE_COMMAND} -E copy "${KERNEL_SYM}" "${INITRAMFS_FILES}/kernel.sym"
  DEPENDS kernel_sym
)
