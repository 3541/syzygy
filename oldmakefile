arch ?= x86_64
target ?= $(arch)-syzygy
kernel := build/kernel-$(arch).bin
syzygy_lib := target/$(target)/debug/libsyzygy.a
iso := build/$(arch).iso

xorriso ?= $(shell whereis xorriso | cut -d' ' -f2)

asm_src := $(wildcard src/arch/$(arch)/*.asm)
asm_obj := $(patsubst src/arch/$(arch)/%.asm, build/arch/$(arch)/%.o, $(asm_src))

nasm_flags ?=
ld_flags ?=
arch_common := $(arch)

ifeq ($(arch), x86_64)
	arch_common := x86_common
	nasm_flags += -felf64
else ifeq ($(arch), i686)
	arch_common := x86_common
	nasm_flags += -felf
	ld_flags += -melf_i386
endif


ldscript := src/arch/$(arch_common)/linker.ld
grub_cfg := src/arch/$(arch_common)/grub.cfg



.PHONY: all clean run iso syzygy

all: $(kernel)

clean:
	rm -r build
	cargo clean

run: $(iso)
	qemu-system-x86_64 -cdrom $(iso)

$(iso): $(kernel) $(grub_cfg)
	mkdir -p build/isofiles/boot/grub
	cp $(kernel) build/isofiles/boot/kernel.bin
	cp $(grub_cfg) build/isofiles/boot/grub
	grub-mkrescue -o $(iso) build/isofiles $(grub_flags) --xorriso=$(xorriso)

$(kernel): syzygy $(asm_obj) $(ldscript) 
	ld $(ld_flags) -n --gc-sections -T $(ldscript) -o $(kernel) $(asm_obj) $(syzygy_lib)

syzygy:
	RUST_TARGET_PATH=$(PWD) xargo build --target=$(target)

build/arch/$(arch)/%.o: src/arch/$(arch)/%.asm $(wildcard src/arch/$(arch_common)/*.asm)
	@mkdir -p $(dir $@)
	nasm $(nasm_flags) $< -o $@ 
