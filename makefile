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
JSDIR = jsbin

# Utility Variables
DIALYZER = $(shell which dialyzer)
ELVIS = $(UTILDIR)/elvis rock --config $(UTILDIR)/elvis.config
ERLPKG = $(UTILDIR)/erlpkg
STDOUT = &1
DEVNULL = /dev/null
GULP = ./node_modules/.bin/gulp
JSLINT = $(GULP) lint

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
	$(call gulp, $(GREEN), $(OUTDIR), default)

nonstrict:
	@ echo "$(GREEN)==> Building RELEASE NONSTRICT$(NORMAL)"
	@ echo "    Any warnings or errors will be ignored during build, if they can be."
	$(call compile, $(ERLFLAGS_NONSTRICT), $(OUTDIR), $(GREEN), $(STDOUT))
	$(call package, $(OUTDIR), $(GREEN))
	$(call gulp, $(GREEN), $(OUTDIR), default)

debug:
	@ echo "$(BLUE)==> Building DEBUG$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	$(call compile, $(ERLFLAGS_DEBUG), $(DEBUGDIR), $(BLUE), $(STDOUT))
	$(call package, $(DEBUGDIR), $(BLUE))
	$(call gulp, $(BLUE), $(DEBUGDIR), debug)

test:
	@ echo "$(CYAN)==> Building TEST$(NORMAL)"
	@ echo "    Compiling with debug options enabled."
	@ echo "    Debug macro enabled."
	@ echo "    Test macro enabled."
	$(call compile, $(ERLFLAGS_TEST), $(TESTDIR), $(CYAN), $(STDOUT))
	$(call gulp, $(CYAN), $(TESTDIR), debug)

js:
	@ echo "$(GREEN)==> Building Jarlang Browser Runtime$(NORMAL)"
	$(call gulp, $(GREEN), $(JSDIR), debug)

.PHONY: lint
lint:
	@ echo "$(PURPLE)==> Linting Erlang Project with Elvis$(NORMAL)"
	@ $(ELVIS) || true
	@ echo "    ok"
	@ echo "$(PURPLE)==> Linting JS Project with JSHint$(NORMAL)"
	@ $(JSLINT) || true
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
	@ find . -name "*.est" -not -path "./doc/*" -delete
	@ echo "==> Removing all EST files from workspace"
	@ find . -name "*.jst" -not -path "./doc/*" -delete
	@ echo "==> Removing all JST files from workspace"
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
	@ rm -rf $(JSDIR)
	@ echo "==> Removing $(JSDIR)/"
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
	@ rm -rf $(OUTPUT_DIR)/*

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

	@ $(call escodegen, $(COLOR), $(OUTPUT_DIR))

	@ echo "$(COLOR)==> Compiling complete in: './$(OUTPUT_DIR)/'$(NORMAL)"
	@ echo "    Done\n"
endef

define package
	@ $(eval OUTPUT_DIR = $(1))
	@ $(eval COLOR = $(2))
	@ echo "$(COLOR)==> Creating erlpkg package in './$(OUTPUT_DIR)/'$(NORMAL)"
	@ cd $(OUTPUT_DIR); ../$(ERLPKG) *.beam *.js node_modules -e jarlang -o jarlang
	@ chmod +x $(OUTPUT_DIR)/jarlang

	@ echo "$(COLOR)==> Cleaning directory './$(OUTPUT_DIR)/'$(NORMAL)"
	@ rm $(OUTPUT_DIR)/*.beam
	@ rm $(OUTPUT_DIR)/*.js
	@ rm -rf $(OUTPUT_DIR)/node_modules
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

define gulp
	@ $(eval COLOR = $(1))
	@ $(eval TARGET_DIR = $(2))
	@ $(eval GULPTASK = $(3))
	@ mkdir -p $(TARGET_DIR)
	@ echo "$(COLOR)==> Running GULP tasks$(NORMAL)"
	@ $(GULP) $(GULPTASK)
	@ echo "$(COLOR)==> Writing JS runtime to location: './$(TARGET_DIR)'$(NORMAL)"
	@ mv gulpbuild/* $(TARGET_DIR)/
	@ rm -rf gulpbuild/
	@ echo "    Done\n"
endef

define escodegen
	@ $(eval COLOR = $(1))
	@ $(eval TARGET_DIR = $(2))
	@ echo "$(COLOR)==> Building ESCODEGEN node modules for packaging$(NORMAL)"
	@ mkdir -p $(TARGET_DIR)/node_modules
	@ echo "$(COLOR)==> Processing ESPRIMA$(NORMAL)"
	@ cp -r node_modules/esprima $(TARGET_DIR)/node_modules/esprima
	@ echo "    ok"	
	@ echo "$(COLOR)==> Processing ESTRAVERSE$(NORMAL)"
	@ cp -r node_modules/estraverse $(TARGET_DIR)/node_modules/estraverse
	@ echo "    ok"	
	@ echo "$(COLOR)==> Processing ESUTILS$(NORMAL)"
	@ cp -r node_modules/esutils $(TARGET_DIR)/node_modules/esutils
	@ echo "    ok"	
	@ echo "$(COLOR)==> Processing OPTIONATOR$(NORMAL)"
	@ cp -r node_modules/optionator $(TARGET_DIR)/node_modules/optionator
	@ echo "    ok"	
	@ echo "$(COLOR)==> Processing SOURCE-MAP$(NORMAL)"
	@ cp -r node_modules/source-map $(TARGET_DIR)/node_modules/source-map
	@ echo "    ok"	
	@ echo "$(COLOR)==> Processing ESCODEGEN$(NORMAL)"
	@ cp -r node_modules/escodegen $(TARGET_DIR)/node_modules/escodegen
	@ echo "    ok"	
endef