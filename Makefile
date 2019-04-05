arch ?= x86_64
target ?= $(arch)-syzygy
debug ?=
build_type ?= debug

qemu := qemu-system-$(arch)
qemu_memory ?= 4G

arch_common := $(arch)
nasm_flags ?=
ld_flags ?=
qemu_flags ?= 
xargo_flags ?=
debug ?=

ifeq ($(arch), x86_64)
	arch_common := x86_common
	nasm_flags += -felf64
else ifeq ($(arch), i686)
	arch_common := x86_common
	ld_flags += -melf_i386
	nasm_flags += -felf
	qemu := qemu-system-i386
endif

ifeq ($(build_type), release)
	xargo_flags += --release
	debug = false
else
	nasm_flags += -wno-number-overflow
endif

ifeq ($(debug), true)
	qemu_flags += -S
endif


libkernel := target/$(target)/$(build_type)/libsyzygy.a
kernel := build/kernel-$(arch)-$(build_type).bin
kernel_debug := build/kernel-$(arch).sym
iso := build/$(arch).iso

asm_src := $(wildcard src/arch/$(arch)/*.asm)
asm_obj := $(patsubst src/arch/$(arch)/%.asm, build/arch/$(arch)/%.o, $(asm_src))
rust_src := $(shell find src/ -type f -name '*.rs')
ldscript := src/arch/$(arch)/linker.ld
grub_cfg := src/arch/$(arch_common)/grub.cfg

integration_test_src := $(shell find src/integration_tests -type f \( -name '*.rs' ! -name 'mod.rs' \))
integration_tests := $(basename $(notdir $(integration_test_src)))


.PHONY: all clean run test

all: $(iso)

clean:
	rm -r build
	cargo clean

run: $(iso)
	@echo [run] $(iso)
	$(qemu) -cdrom $(iso) -s -serial mon:stdio -m $(qemu_memory) -device isa-debug-exit,iobase=0xF4,iosize=0x04 $(qemu_flags)

test: xargo_flags += --features 'integration-tests'
test: export RUSTFLAGS=-A dead_code -A unused_imports -A unused_variables
test: rust_src += $(integration_test_src)
#test: kernel := build/kernel-$(arch)-$(build_type)-test.bin
test: kernel := $(basename $(kernel))-test.bin
test: iso := $(basename $(iso))-test.iso
test: temp := $(shell mktemp -d)
test: qemu_pipe := $(temp)/qemu_pipe
test: $(rust_src) $(iso) 
	@echo [test] unit tests
	cargo test --target $(arch)-unknown-linux-gnu
	@echo [test] integration tests
	for t in $(integration_tests); do 					\
		echo [integration test] $$t; 					\
		mkfifo $(qemu_pipe).in;							\
		mkfifo $(qemu_pipe).out;						\
		$(qemu) -cdrom $(iso) -s -chardev pipe,id=ch0,path=$(qemu_pipe) -serial chardev:ch0	-m 4G -device isa-debug-exit,iobase=0xF4,iosize=0x04 &	\
		cat $(qemu_pipe).out > $(qemu_pipe).out_nb &	\
		sleep 2;										\
		echo $$t_ > $(qemu_pipe).in;					\
	done

$(iso): $(kernel) $(grub_cfg)
	@echo [build] $(iso)
	mkdir -p build/isofiles/boot/grub
	cp $(kernel) build/isofiles/boot/kernel.bin
	cp $(grub_cfg) build/isofiles/boot/grub
	grub-mkrescue -o $(iso) build/isofiles 2> /dev/null

$(kernel): $(asm_obj) $(ldscript) $(libkernel)
	@echo [link] $(kernel)
	ld $(ld_flags) -n --gc-sections -T $(ldscript) -o $(kernel) $(asm_obj) $(libkernel)

$(libkernel): $(rust_src) $(target).json
	@echo [build] $@
	RUST_TARGET_PATH=$(PWD) xargo build --target $(target) $(xargo_flags)

build/arch/$(arch)/%.o: src/arch/$(arch)/%.asm $(wildcard src/arch/$(arch_common)/*.asm)
	@echo [build] $@
	@mkdir -p $(dir $@)
	nasm $(nasm_flags) $< -o $@ 

#.SILENT:
