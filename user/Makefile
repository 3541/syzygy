include $(BUILD_ROOT)/make/common.mk

.PHONY: all clean

all: $(userland_all)

$(userland_test): $(userland_test_obj)
	@echo [link] $(notdir $@)
	$(quiet)ld $(ld_flags) -o $@ $(userland_test_obj)

$(OUT)/user/test/%.o: $(BUILD_ROOT)/user/test/%.asm
	@echo [build] $(notdir $@)
	@echo mkdir -p $(dir $@)
	$(quiet)nasm $(nasm_flags) $< -o $@
