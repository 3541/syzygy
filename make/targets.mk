# This file defines all build targets in the project. It should be included by most things.

OUT ?= $(BUILD_ROOT)/build

# Top level
iso := $(OUT)/$(arch)-$(build_type).iso

initramfs := $(OUT)/initramfs-$(arch)-$(build_type).fs
mkinitramfs := $(OUT)/$(build_type)/mk
mkinitramfs_src := $(shell find $(BUILD_ROOT)/initramfs/src -type f)
initramfs_base := $(BUILD_ROOT)/initramfs/fs
initramfs_files := $(shell find $(initramfs_base) -type f)

# kernel/
kernel := $(OUT)/kernel-$(arch)-$(build_type).elf
kernel_symbols := $(OUT)/kernel-$(arch)-$(build_type).sym
initramfs_files += $(kernel_symbols)
kernel_src := $(shell find $(BUILD_ROOT)/kernel/src -type f)

libkernel := $(OUT)/$(target)/$(build_type)/libsyzygy.a
libkernel_asm_src := $(wildcard $(BUILD_ROOT)/kernel/src/arch/$(arch)/*.asm)
libkernel_asm_obj := $(patsubst $(BUILD_ROOT)/kernel/src/arch/$(arch)/%.asm, $(OUT)/arch/$(arch)/%.o, $(libkernel_asm_src))
libkernel_rust_src := $(shell find $(BUILD_ROOT)/kernel/src/ -type f -name '*.rs')
libkernel_ldscript := $(BUILD_ROOT)/kernel/src/arch/$(arch)/linker.ld
