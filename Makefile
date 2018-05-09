arch ?= x86_64
kernel := build/kernel-$(arch).bin
iso := build/$(arch).iso

xorriso ?= $(shell whereis xorriso | cut -d' ' -f2)

asm_src := $(wildcard src/arch/$(arch)/*.asm)
asm_obj := $(patsubst src/arch/$(arch)/%.asm, build/arch/$(arch)/%.o, $(asm_src))

nasm_flags ?=
ld_flags ?=
arch_common := $(arch)

ld_i386 := elf_i386
ifeq ($(shell uname), FreeBSD)
	ld_i386 := elf_i386_fbsd
endif

ifeq ($(arch), x86_64)
	arch_common := x86_common
	nasm_flags += -felf64
else ifeq ($(arch), i686)
	arch_common := x86_common
	nasm_flags += -felf
	ld_flags += -m$(ld_i386)
endif


ldscript := src/arch/$(arch_common)/linker.ld
grub_cfg := src/arch/$(arch_common)/grub.cfg



.PHONY: all clean run iso

all: $(kernel)

clean:
	rm -r build

run: $(iso)
	qemu-system-x86_64 -cdrom $(iso)

$(iso): $(kernel) $(grub_cfg)
	mkdir -p build/isofiles/boot/grub
	cp $(kernel) build/isofiles/boot/kernel.bin
	cp $(grub_cfg) build/isofiles/boot/grub
	grub-mkrescue -o $(iso) build/isofiles $(grub_flags) --xorriso=$(xorriso)

$(kernel): $(asm_obj) $(ldscript) 
	ld $(ld_flags) -n -T $(ldscript) -o $(kernel) $(asm_obj)

build/arch/$(arch)/%.o: src/arch/$(arch)/%.asm $(wildcard src/arch/$(arch_common)/*.asm)
	@mkdir -p $(dir $@)
	nasm $(nasm_flags) $< -o $@ 
