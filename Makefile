BUILD_ROOT := $(CURDIR)

include ./make/common.mk

qemu := qemu-system-$(arch)
qemu_memory ?= 4G

ifneq (, $(shell which grub-mkrescue 2> /dev/null))
	grub_mkrescue := grub-mkrescue
else ifneq (, $(shell which grub2-mkrescue 2> /dev/null))
	grub_mkrescue := grub2-mkrescue
else
	$(error Cannot find grub-mkrescue)
endif


ifeq ($(arch), i686)
	qemu := qemu-system-i386
endif



ifeq ($(debug), true)
	qemu_flags += -S
endif

gdb := tools/bin/gdb

# Sources
grub_cfg := kernel/src/arch/$(arch_common)/grub.cfg


common_deps :=  $(PWD)/Cargo.lock $(PWD)/Xargo.toml

targets := $(shell find $(PWD)/targets/ -type f)
common_deps += $(targets)

# For the submake
export arch target build_type arch_common nasm_flags ld_flags xargo_flags rustc_flags quiet BUILD_ROOT


.PHONY: all clean run test tools

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

$(iso): $(kernel) $(grub_cfg) $(initramfs)
	@echo [build] $(iso)
	@mkdir -p build/isofiles/boot/grub
	$(quiet)cp $(kernel) build/isofiles/boot/kernel.bin
	$(quiet)cp $(grub_cfg) build/isofiles/boot/grub
	$(quiet)cp $(initramfs) build/isofiles/boot/initramfs.fs
	$(quiet)$(grub_mkrescue) -o $(iso) build/isofiles 2> /dev/null

$(kernel): $(kernel_src) $(mkinitramfs_src)
	@echo [submake] kernel
	$(quiet)$(MAKE) -C kernel/ DEPS="$(common_deps) $(mkinitramfs_src)"

$(kernel_symbols): $(kernel)
	@echo [build] kernel symbols
	$(quiet)nm $(kernel) | sort -r > $@

$(initramfs): $(mkinitramfs) $(initramfs_files)
	@echo [build] initramfs
	@cp -r $(initramfs_base) build/
	@cp $(kernel_symbols) build/fs/kernel.sym
	@cp $(userland_test) build/fs/userland_test
	$(quiet)$(mkinitramfs) build/fs $@

$(mkinitramfs): $(mkinitramfs_src)
	@echo [build] mkinitramfs
	$(quiet)RUST_TARGET_PATH="$(targets)" RUSTFLAGS="$(rustc_flags)" CARGO_TARGET_DIR="build" cargo build -p initramfs

$(userland_test): $(userland_test_src)
	@echo [submake] userland_test
	$(quiet)$(MAKE) -C user/ $(userland_test)

tools: $(gdb)
	@echo [build] tools

$(gdb):
	@echo [submake] gdb
	$(quiet) make -C tools/gdb
