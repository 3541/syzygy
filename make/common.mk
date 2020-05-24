# Common definitions

ifdef verbose
	quiet ?=
else
	quiet ?= @
endif

BUILD_ROOT ?= $(CURDIR)/..
OUT ?= $(BUILD_ROOT)/build
TARGETS ?= $(BUILD_ROOT)/targets
DEPS ?=

arch ?= x86_64
target ?= $(arch)-elf
debug ?=
build_type ?= debug
ident ?= $(target)-$(build_type)

arch_common := $(arch)
nasm_flags ?=
ld_flags ?=
qemu_flags ?=
xargo_flags ?=
rustc_flags ?= -Zsymbol-mangling-version=v0 -Cforce-frame-pointers=yes

ifeq ($(arch), x86_64)
	arch_common := x86_common
	nasm_flags += -felf64
else ifeq ($(arch), i686)
	arch_common := x86_common
	ld_flags += -melf_i386
	nasm_flags += -felf
endif

ifeq ($(build_type), release)
	xargo_flags += --release
	debug = false
else
	nasm_flags += -wno-number-overflow
endif

include $(BUILD_ROOT)/make/targets.mk