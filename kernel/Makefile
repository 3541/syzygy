# THIS IS A SUBMAKE FILE. IT SHOULD BE INVOKED FROM ELSEWHERE
BUILD_ROOT ?= $(PWD)/..
OUT ?= $(BUILD_ROOT)/build
TARGETS ?= $(BUILD_ROOT)/targets
DEPS ?=

arch ?= x86_64
target ?= $(arch)-syzygy
debug ?=
build_type ?= debug

arch_common := $(arch)
nasm_flags ?=
ld_flags ?=
qemu_flags ?=
xargo_flags ?=
rustc_flags ?=

ifeq ($(arch), x86_64)
	arch_common := x86_common
	nasm_flags += -felf64
else ifeq ($(arch), i686)
	arch_common := x86_common
	ld_flags += -melf_i386
	nasm_flags += -felf
endif

# Build targets
libkernel := $(OUT)/$(target)/$(build_type)/libsyzygy.a
kernel ?= $(OUT)/kernel-$(arch)-$(build_type).elf

asm_src := $(wildcard src/arch/$(arch)/*.asm)
asm_obj := $(patsubst src/arch/$(arch)/%.asm, $(OUT)/arch/$(arch)/%.o, $(asm_src))
rust_src := $(shell find src/ -type f -name '*.rs')
ldscript := src/arch/$(arch)/linker.ld

integration_tests_src := $(shell find src/integration_tests -type f \( -name '*.rs' ! -name 'mod.rs' \))
integration_tests := $(basename $(notdir $(integration_tests_src)))
ifeq ($(build_type), test)
	xargo_flags += --features 'integration-tests'
	rustc_flags += -A dead_code -A unused_imports -A unused_variables
	libkernel := $(OUT)/$(target)/debug/libsyzygy.a
endif

.PHONY: all clean

all: $(kernel)
	@mkdir -p build

clean:
	@echo [clean] $(PWD)
	$(quiet)cargo clean

$(kernel): $(asm_obj) $(ldscript) $(libkernel)
	@echo [link] $(notdir $@)
	$(quiet)ld $(ld_flags) -n --gc-sections -T $(ldscript) -o $(kernel) $(asm_obj) $(libkernel)

$(libkernel): $(rust_src) $(DEPS)
	@echo [build] $(notdir $@)
	$(quiet)cd $(BUILD_ROOT) && \
	RUST_TARGET_PATH="$(TARGETS)" RUSTFLAGS="$(rustc_flags)" CARGO_TARGET_DIR="$(OUT)" \
	xargo build --target $(target) $(xargo_flags) -p syzygy

$(OUT)/arch/$(arch)/%.o: src/arch/$(arch)/%.asm $(wildcard src/arch/$(arch_common)/*.asm)
	@echo [build] $(notdir $@)
	@mkdir -p $(dir $@)
	$(quiet)nasm $(nasm_flags) $< -o $@
