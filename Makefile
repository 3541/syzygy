arch ?= x86_64
target ?= $(arch)-syzygy
debug ?=
build_type ?= debug

qemu := qemu-system-$(arch)

arch_common := $(arch)
nasm_flags ?=
ld_flags ?=
qemu_flags ?=

ifeq ($(arch), x86_64)
	arch_common := x86_common
	nasm_flags += -felf64
else ifeq ($(arch), i686)
	arch_common := x86_common
	ld_flags += -melf_i386
	nasm_flags += -felf
	qemu := qemu-system-i386
endif

ifeq ($(debug), true)
	qemu_flags += -s -S
endif

ifeq ($(build_type), release)
	# things
endif

libkernel := target/$(target)/$(build_type)/libsyzygy.a
kernel := build/kernel-$(arch).bin
iso := build/$(arch).iso

asm_src := $(wildcard src/arch/$(arch)/*.asm)
asm_obj := $(patsubst src/arch/$(arch)/%.asm, build/arch/$(arch)/%.o, $(asm_src))
rust_src := $(shell find src/ -type f -name '*.rs')
ldscript := src/arch/$(arch)/linker.ld
grub_cfg := src/arch/$(arch_common)/grub.cfg

.PHONY: all clean run

all: $(kernel)

clean:
	rm -r build
	cargo clean

run: $(iso)
	$(qemu) -cdrom $(iso) $(qemu_flags)

$(iso): $(kernel) $(grub_cfg)
	mkdir -p build/isofiles/boot/grub
	cp $(kernel) build/isofiles/boot/kernel.bin
	cp $(grub_cfg) build/isofiles/boot/grub
	grub-mkrescue -o $(iso) build/isofiles 2> /dev/null

$(kernel): $(asm_obj) $(ldscript) $(libkernel)
	ld $(ld_flags) -n --gc-sections -T $(ldscript) -o $(kernel) $(asm_obj) $(libkernel)

$(libkernel): $(rust_src) $(target).json
	RUST_TARGET_PATH=$(PWD) xargo build --target $(target)

build/arch/$(arch)/%.o: src/arch/$(arch)/%.asm $(wildcard src/arch/$(arch_common)/*.asm)
	@mkdir -p $(dir $@)
	nasm $(nasm_flags) $< -o $@ 

.SILENT:
