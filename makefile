SHELL = /bin/sh

# Compilation Variables
ERLC = $(shell which erlc)
ERLFLAGS = -Werror -v -o
ERLFLAGS_NONSTRICT = -Wall -v -o
ERLFLAGS_DEBUG = -Ddebug +debug_info -W0 -o
JSMINIFY = cp
JSMINIFYFLAGS = -f

# Directory Variables
SRCDIR = src
LIBDIR = src/lib
OUTDIR = ebin
DEBUGDIR = edebug
TESTDIR = etesting
UTILDIR = util

# Utility Variables
DIALYZER = $(shell which dialyzer)
ELVIS = $(UTILDIR)/elvis rock --config $(UTILDIR)/elvis.config
ERLPKG = $(UTILDIR)/erlpkg

# Colors
RED = \033[0;31m
GREEN = \033[0;32m
BLUE = \033[0;34m
ORANGE = \033[0;33m
PURPLE = \033[0;35m
CYAN = \033[0;36m
NORMAL = \033[0m

define compile
	@ $(eval COMPILE_MODE = $(1))
	@ $(eval OUTPUT_DIR = $(2))
	@ $(eval COLOR = $(3))
	@ echo "    Compiling files with debug_info enabled"
	@ echo "    Compiling files with warnings being considered errors"
	@ echo "    Compiling files will fail if any errors occur"
	@ mkdir -p $(OUTPUT_DIR)
	@ rm -f $(OUTPUT_DIR)/*

	@ echo "$(COLOR)==> Compiling Library Files$(RED)"
	@ $(ERLC) $(COMPILE_MODE) $(OUTPUT_DIR) $(LIBDIR)/*.erl
	@ echo "$(NORMAL)    Done"

	@ echo "$(COLOR)==> Compiling Source Files$(RED)"
	@ $(ERLC) $(COMPILE_MODE) $(OUTPUT_DIR) $(SRCDIR)/*.erl
	@ echo "$(NORMAL)    Done"

	@ echo "$(COLOR)==> Bootstrapping NodeJS Environment onto build$(RED)"
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(SRCDIR)/*.js $(OUTPUT_DIR) 2> /dev/null || true
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(LIBDIR)/*.js $(OUTPUT_DIR) 2> /dev/null || true
	@ echo "$(NORMAL)    Done"

	@ echo "$(COLOR)==> Compiling complete in: './$(OUTPUT_DIR)/'$(NORMAL)"
	@ echo "    Done\n"
endef

define package
	@ $(eval OUTPUT_DIR = $(1))
	@ $(eval COLOR = $(2))
	@ echo "$(COLOR)==> Creating erlpkg package in './$(OUTPUT_DIR)/'$(NORMAL)"
	@ $(ERLPKG) $(OUTPUT_DIR)/*.beam $(OUTPUT_DIR)/*.js node_modules/ -e jarlang -o $(OUTPUT_DIR)/jarlang

	@ echo "$(COLOR)==> Cleaning directory './$(OUTPUT_DIR)/'$(NORMAL)"
	@ rm $(OUTPUT_DIR)/*.beam
	@ rm $(OUTPUT_DIR)/*.js

	@ echo "$(COLOR)==> Packaging complete in: './$(OUTPUT_DIR)/'$(NORMAL)"
	@ echo "    Done\n"
endef

define dialyze
	@ $(eval TARGET_DIR = $(1))
	@ $(eval COLOR = $(2))
	@ echo "$(COLOR)==> Running Dialyzer$(NORMAL)"
	@ $(DIALYZER) $(TARGET_DIR)/*.beam || true

	@ echo "$(COLOR)==> Dialyzing complete in: './$(TARGET_DIR)/'$(NORMAL)"
	@ echo "    Done\n"
endef

release:
	@ echo "$(GREEN)==> Building RELEASE$(NORMAL)"
	$(call compile, $(ERLFLAGS), $(OUTDIR), $(GREEN))
	$(call package, $(OUTDIR), $(GREEN))

nonstrict:
	@ echo "$(GREEN)==> Building RELEASE NONSTRICT$(NORMAL)"
	$(call compile, $(ERLFLAGS_NONSTRICT), $(OUTDIR), $(GREEN))
	$(call package, $(OUTDIR), $(GREEN))

debug:
	@ echo "$(BLUE)==> Building DEBUG$(NORMAL)"
	$(call compile, $(ERLFLAGS_DEBUG), $(DEBUGDIR), $(BLUE))
	$(call package, $(DEBUGDIR), $(BLUE))

.PHONY: lint
lint:
	@ echo "$(ORANGE)==> Linting Project with Elvis$(NORMAL)"
	@ $(ELVIS) || true
	@ echo "    Done\n"

.PHONY: dialyze
dialyze:
	@ echo "$(ORANGE)==> Building DIALYZE$(NORMAL)"
	$(call compile, $(ERLFLAGS_DEBUG), $(TESTDIR), $(ORANGE))
	$(call dialyze, $(TESTDIR), $(ORANGE))

.PHONY: clean
clean:
	@ echo "$(ORANGE)==> Cleaning builds"
	@ find . -name "*.beam" -not -path "./doc/*" -delete 
	@ echo "==> Removing all BEAM files from workspace"
	@ find . -name "*.core" -not -path "./doc/*" -delete 
	@ echo "==> Removing all CORE files from workspace"
	@ find . -name "*.ast" -not -path "./doc/*" -delete 
	@ echo "==> Removing all AST files from workspace"
	@ find . -name "*.dump" -not -path "./doc/*" -delete 
	@ echo "==> Removing all DUMP files from workspace"
	@ rm -rf $(OUTDIR)
	@ echo "==> Removing $(OUTDIR)/"
	@ rm -rf $(DEBUGDIR)
	@ echo "==> Removing $(DEBUGDIR)/"
	@ rm -rf $(TESTDIR)
	@ echo "==> Removing $(TESTDIR)/"
	@ echo "==> Cleaned\n$(NORMAL)"