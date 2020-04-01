ifdef verbose
	quiet ?=
else
	quiet ?= @
endif

arch ?= x86_64
target ?= $(arch)-syzygy
debug ?=
build_type ?= debug

qemu := qemu-system-$(arch)
qemu_memory ?= 4G

ifneq (, $(shell which grub-mkrescue 2> /dev/null))
	grub_mkrescue := grub-mkrescue
else ifneq (, $(shell which grub2-mkrescue 2> /dev/null))
	grub_mkrescue := grub2-mkrescue
else
	$(error Cannot find grub-mkrescue)
endif

arch_common := $(arch)
nasm_flags ?=
ld_flags ?=
qemu_flags ?=
xargo_flags ?=
rustc_flags ?=

rustc_flags += -Zsymbol-mangling-version=v0

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
	rustc_flags += -Cforce-frame-pointers=yes
endif


ifeq ($(debug), true)
	qemu_flags += -S
endif


# Build targets
iso := build/$(arch)-$(build_type).iso
kernel := build/kernel-$(arch)-$(build_type).elf


# Sources
grub_cfg := kernel/src/arch/$(arch_common)/grub.cfg


common_deps := $(shell find $(PWD)/targets/ -type f) $(PWD)/Cargo.lock $(PWD)/Xargo.toml

# For the submake
export arch target build_type arch_common nasm_flags ld_flags xargo_flags rust_flags quiet


.PHONY: all clean run test $(kernel)

all: $(iso)
	$(quiet)mkdir -p build

clean:
	@echo [clean] all
	$(quiet)rm -r build
	$(quiet)cargo clean
	$(quiet)$(MAKE) -C kernel/ clean

run: $(iso)
	@echo [run] $(iso)
	$(quiet)$(qemu) -cdrom $(iso) -s -serial mon:stdio -m $(qemu_memory) -device isa-debug-exit,iobase=0xF4,iosize=0x04 $(qemu_flags)

test: temp := $(shell mktemp -d)
test: qemu_pipe := $(temp)/qemu_pipe
test: libkernel := build/$(target)/debug/libsyzygy.a
test: $(rust_src)
	@echo [test] unit tests
	$(quiet)cargo test --target $(arch)-unknown-linux-gnu
	@echo [test] integration tests
	@echo [awful hack] swap $(libkernel)
	@-mv $(libkernel){,.bk} &> /dev/null
	@-mv $(libkernel){.t,} &> /dev/null
	$(MAKE) -C kernel/ DEPS="$(common_deps)" BUILD_ROOT="$(PWD)" build_type=test
	@echo [awful hack] revert
	@-mv $(libkernel){,.t} &> /dev/null
	@- mv $(libkernel){.bk,} &> /dev/null
	@echo [create] $(qemu_pipe)
	$(quiet)mkfifo $(qemu_pipe).in
	$(quiet)mkfifo $(qemu_pipe).out
	$(quiet)for t in $(integration_tests); do					\
		echo [integration test] $$t;						\
		cat $(qemu_pipe).out > $(qemu_pipe).out_nb &				\
		echo "a$${t}_" > $(qemu_pipe).in &					\
		$(qemu) -cdrom build/$(arch)-test.iso -s -chardev pipe,id=ch0,path=$(qemu_pipe) -serial chardev:ch0 -m 4G -device isa-debug-exit,iobase=0xF4,iosize=0x04;	\
		status=$$(cat $(qemu_pipe).out_nb);					\
		echo $$status;								\
		case $$status in							\
			*ok) ;;								\
			*) exit 255;;							\
		esac;									\
	done
	@echo [clean] $(temp)
	$(quiet)rm -r $(temp)

$(iso): $(kernel) $(grub_cfg)
	@echo [build] $(iso)
	@mkdir -p build/isofiles/boot/grub
	$(quiet)cp $(kernel) build/isofiles/boot/kernel.bin
	$(quiet)cp $(grub_cfg) build/isofiles/boot/grub
	$(quiet)$(grub_mkrescue) -o $(iso) build/isofiles 2> /dev/null

$(kernel):
	@echo [submake] kernel
	$(quiet)$(MAKE) -C kernel/ DEPS="$(common_deps)" BUILD_ROOT="$(PWD)"
