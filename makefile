SHELL = /bin/sh

# ========================== #
# === Makefile Variables === #
# ========================== #

# Compilation Variables
ERLC = $(shell which erlc)
ERLFLAGS = -Werror -v -o
ERLFLAGS_NONSTRICT = -Wall -v -o
ERLFLAGS_DEBUG = -Ddebug +debug_info -W0 -o
ERLFLAGS_TEST = -DTEST -Ddebug +debug_info -W0 -o
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
STDOUT = &1
DEVNULL = /dev/null

# Colors
RED = \033[0;31m
GREEN = \033[0;32m
BLUE = \033[0;34m
ORANGE = \033[0;33m
PURPLE = \033[0;35m
CYAN = \033[0;36m
NORMAL = \033[0m

# ====================== #
# === Target recipes === #
# ====================== #

release:
	@ echo "$(GREEN)==> Building RELEASE$(NORMAL)"
	@ echo "    Any warnings or errors will stop the build."
	$(call compile, $(ERLFLAGS), $(OUTDIR), $(GREEN), $(DEVNULL))
	$(call package, $(OUTDIR), $(GREEN))

nonstrict:
	@ echo "$(GREEN)==> Building RELEASE NONSTRICT$(NORMAL)"
	@ echo "    Any warnings or errors will be ignored during build, if they can be."
	$(call compile, $(ERLFLAGS_NONSTRICT), $(OUTDIR), $(GREEN), $(STDOUT))
	$(call package, $(OUTDIR), $(GREEN))

debug:
	@ echo "$(BLUE)==> Building DEBUG$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	$(call compile, $(ERLFLAGS_DEBUG), $(DEBUGDIR), $(BLUE), $(STDOUT))
	$(call package, $(DEBUGDIR), $(BLUE))

test:
	@ echo "$(CYAN)==> Building TEST$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	@ echo "    Test macro enabled."
	$(call compile, $(ERLFLAGS_TEST), $(TESTDIR), $(CYAN), $(STDOUT))

.PHONY: lint
lint:
	@ echo "$(PURPLE)==> Linting Project with Elvis$(NORMAL)"
	@ $(ELVIS) || true
	@ echo "    Done\n"

.PHONY: dialyze
dialyze:
	@ echo "$(ORANGE)==> Building DIALYZE$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	@ echo "    Test macro enabled."
	$(call compile, $(ERLFLAGS_DEBUG), $(TESTDIR), $(ORANGE), $(STDOUT))
	$(call dialyze, $(TESTDIR), $(ORANGE))

.PHONY: clean
clean:
	@ echo "$(RED)==> Cleaning builds"
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
	@ echo "    Done"
	@ rm -rf $(DEBUGDIR)
	@ echo "==> Removing $(DEBUGDIR)/"
	@ echo "    Done"
	@ rm -rf $(TESTDIR)
	@ echo "==> Removing $(TESTDIR)/"
	@ echo "    Done"
	@ echo "==> Cleaned\n$(NORMAL)"

# ========================= #
# === Recipe Procedures === #
# ========================= #

define compile
	@ $(eval COMPILE_MODE = $(1))
	@ $(eval OUTPUT_DIR = $(2))
	@ $(eval COLOR = $(3))
	@ $(eval PIPE_TO = $(4))

	@ mkdir -p $(OUTPUT_DIR)
	@ rm -f $(OUTPUT_DIR)/*

	@ echo "$(COLOR)==> Compiling Library Files$(RED)"
	@ $(ERLC) $(COMPILE_MODE) $(OUTPUT_DIR) $(LIBDIR)/*.erl >$(PIPE_TO)
	@ echo "$(NORMAL)    Done"

	@ echo "$(COLOR)==> Compiling Source Files$(RED)"
	@ $(ERLC) $(COMPILE_MODE) $(OUTPUT_DIR) $(SRCDIR)/*.erl >$(PIPE_TO)
	@ echo "$(NORMAL)    Done"

	@ echo "$(COLOR)==> Bootstrapping NodeJS Environment onto build$(RED)"
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(SRCDIR)/*.js $(OUTPUT_DIR) 2>$(DEVNULL) || true
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(LIBDIR)/*.js $(OUTPUT_DIR) 2>$(DEVNULL) || true
	@ echo "$(NORMAL)    Done"

	@ echo "$(COLOR)==> Compiling complete in: './$(OUTPUT_DIR)/'$(NORMAL)"
	@ echo "    Done\n"
endef

define package
	@ $(eval OUTPUT_DIR = $(1))
	@ $(eval COLOR = $(2))
	@ echo "$(COLOR)==> Creating erlpkg package in './$(OUTPUT_DIR)/'$(NORMAL)"
	@ $(ERLPKG) $(OUTPUT_DIR)/*.beam $(OUTPUT_DIR)/*.js node_modules/ -e jarlang -o $(OUTPUT_DIR)/jarlang
	@ chmod +x $(OUTPUT_DIR)/jarlang

	@ echo "$(COLOR)==> Cleaning directory './$(OUTPUT_DIR)/'$(NORMAL)"
	@ rm $(OUTPUT_DIR)/*.beam
	@ rm $(OUTPUT_DIR)/*.js
	@ echo "    Done\n"

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