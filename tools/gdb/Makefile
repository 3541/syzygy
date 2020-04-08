quiet ?= @

BASE ?= $(CURDIR)/../..

build := $(CURDIR)/build
src := $(build)/gdb-9.1.tar.xz
url := "https://ftp.gnu.org/gnu/gdb/$(notdir $(src))"
install := $(BASE)/tools/install
out := $(install)/bin/gdb
patch := $(CURDIR)/fix.patch

.PHONY: all clean

all: $(out)

clean:
	@echo [clean]
	$(quiet)rm -rf $(build)/*
	$(quiet)mkdir -p $(build)

$(out): $(src) $(patch)
	$(quiet)mkdir -p $(build)/work
	@echo [extract] $(src)
	$(quiet)tar xf $(src) -C $(build)
	@echo [patch] $(patch)
	$(quiet)cd $(build) && patch -p0 < $(patch)
	@echo [configure]
	$(quiet)cd $(build)/work && $(build)/gdb-9.1/configure --prefix=$(install)
	@echo [build]
	$(quiet)make -C $(build)/work
	@echo [install]
	$(quiet)make -C $(build)/work install

$(src):
	@echo [fetch] $@
	$(quiet)wget $(url) -O $@
