arch ?= x86_64
target ?= $(arch)-syzygy

arch_common := $(arch)
nasm_flags ?=
ld_flags ?=

ifeq ($(arch), x86_64)
	arch_common := x86_common
	nasm_flags += -felf64
else ifeq ($(arch), i686)
	arch_common := x86_common
	ld_flags += -melf_i386
	nasm_flags += -felf
endif

xorriso ?= $(shell whereis xorriso | cut -d' ' -f2)

kernel := build/kernel-$(arch).bin
iso := build/$(arch).iso

asm_src := $(wildcard src/arch/$(arch)/*.asm)
asm_obj := $(patsubst src/arch/$(arch)/%.asm, build/arch/$(arch)/%.o, $(asm_src))
ldscript := src/arch/$(arch_common)/linker.ld
grub_cfg := src/arch/$(arch_common)/grub.cfg

.PHONY: all clean run

all: $(kernel)

clean:
	rm -r build

run: $(iso)
	qemu-system-x86_64 -cdrom $(iso)

$(iso): $(kernel) $(grub_cfg)
	mkdir -p build/isofiles/boot/grub
	cp $(kernel) build/isofiles/kernel.bin
	cp $(grub_cfg) build/isofiles/boot/grub
	grub-mkrescue -o $(iso) build/isofiles --xorriso=$(xorriso)

$(kernel): $(asm_obj) $(ldscript)
	ld $(ld_flags) -n --gc-sections -T $(ldscript) -o $(kernel) $(asm_obj)

build/arch/$(arch)/%.o: src/arch/$(arch)/%.asm $(wildcard src/arch/$(arch_common)/*.asm)
	@mkdir -p $(dir $@)
	nasm $(nasm_flags) $< -o $@ 
