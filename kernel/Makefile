# THIS IS A SUBMAKE FILE. IT SHOULD BE INVOKED FROM ELSEWHERE
include $(BUILD_ROOT)/make/common.mk

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
	@echo [clean] $(CURDIR)
	$(quiet)cargo clean

$(kernel): $(libkernel_asm_obj) $(libkernel_ldscript) $(libkernel)
	@echo [link] $(notdir $@)
	$(quiet)ld $(ld_flags) -n --gc-sections -T $(libkernel_ldscript) -o $@ $(libkernel_asm_obj) $(libkernel)

$(libkernel): $(libkernel_rust_src) $(DEPS)
	@echo [build] $(notdir $@)
	$(quiet)cd $(BUILD_ROOT) && \
	RUST_TARGET_PATH="$(TARGETS)" RUSTFLAGS="$(rustc_flags)" CARGO_TARGET_DIR="$(OUT)" \
	xargo build --target $(target) $(xargo_flags) -p syzygy

$(OUT)/arch/$(arch)/%.o: src/arch/$(arch)/%.asm $(wildcard src/arch/$(arch_common)/*.asm)
	@echo [build] $(notdir $@)
	@mkdir -p $(dir $@)
	$(quiet)nasm $(nasm_flags) $< -o $@
