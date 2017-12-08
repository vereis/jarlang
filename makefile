SHELL = /bin/sh

# Compilation Variables
ERLC = $(shell which erlc)
ERLFLAGS = -Wall -v -o
DEBUGFLAGS = +debug_info -W0 -o
JSMINIFY = cp
JSMINIFYFLAGS = -f

# Directory Variables
SRCDIR = src
LIBDIR = src/lib
MISCDIR = misc
OUTDIR = ebin
DEBUGDIR = edebug
TESTDIR = etesting

# Colors
RED = \033[0;31m
GREEN = \033[0;32m
BLUE = \033[0;34m
ORANGE = \033[0;33m
PURPLE = \033[0;35m
CYAN = \033[0;36m
NORMAL = \033[0m

release:
	@ echo "$(GREEN)==> Building RELEASE$(NORMAL)"
	@ echo "    Compiling files with debug_info enabled"
	@ echo "    Compiling files with warnings being considered errors"
	@ echo "    Compiling files will fail if any errors occur"
	@ mkdir -p $(OUTDIR)
	@ rm -f $(OUTDIR)/*.beam
	@ rm -f $(OUTDIR)/*.sh
	@ rm -f $(OUTDIR)/*.js
	@ echo "$(GREEN)==> Compiling Library Files$(RED)"
	@ $(ERLC) $(ERLFLAGS) $(OUTDIR) $(LIBDIR)/*.erl
	@ echo "$(NORMAL)    Done"
	@ echo "$(GREEN)==> Compiling Source Files$(RED)"
	@ $(ERLC) $(ERLFLAGS) $(OUTDIR) $(SRCDIR)/*.erl
	@ echo "$(NORMAL)    Done"
	@ echo "$(GREEN)==> Bootstrapping NodeJS Environment onto build$(RED)"
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(SRCDIR)/*.js $(OUTDIR) 2> /dev/null || true
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(LIBDIR)/*.js $(OUTDIR) 2> /dev/null || true
	@ echo "$(NORMAL)    Done"
	@ echo "$(GREEN)==> Creating launch script in './$(OUTDIR)/'$(NORMAL)"
	@ cp -f $(MISCDIR)/* $(OUTDIR) 2> /dev/null || true
	@ echo "$(NORMAL)    Done"
	@ echo "$(GREEN)==> RELEASE release successfully built in './$(OUTDIR)/'$(NORMAL)"
	@ echo "    You can run jarlang with './$(OUTDIR)/jarlang.sh FILE1 FILE2 FILE3...'"
	@ echo "    Done\n"
debug:
	@ echo "$(BLUE)==> Building DEBUG$(NORMAL)"
	@ echo "    Compiling files with debug_info enabled"
	@ echo "    Compiling files with warnings ignored"
	@ echo "    Compiling files will fail if any errors occur"
	@ mkdir -p $(DEBUGDIR)
	@ rm -f $(DEBUGDIR)/*.beam
	@ rm -f $(DEBUGDIR)/*.sh
	@ rm -f $(DEBUGDIR)/*.js
	@ echo "$(BLUE)==> Compiling Library Files$(RED)"
	@ $(ERLC) $(DEBUGFLAGS) $(DEBUGDIR) $(LIBDIR)/*.erl
	@ echo "$(NORMAL)    Done"
	@ echo "$(BLUE)==> Compiling Source Files$(RED)"
	@ $(ERLC) $(DEBUGFLAGS) $(DEBUGDIR) $(SRCDIR)/*.erl
	@ echo "$(NORMAL)    Done"
	@ echo "$(BLUE)==> Bootstrapping NodeJS Environment onto build$(RED)"
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(SRCDIR)/*.js $(DEBUGDIR) 2> /dev/null || true
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(LIBDIR)/*.js $(DEBUGDIR) 2> /dev/null || true
	@ echo "$(NORMAL)    Done"
	@ echo "$(BLUE)==> Creating launch script in './$(DEBUGDIR)/'$(NORMAL)"
	@ cp -f $(MISCDIR)/* $(DEBUGDIR) 2> /dev/null || true
	@ echo "$(NORMAL)    Done"
	@ echo "$(BLUE)==> DEBUG release successfully built in './$(DEBUGDIR)/'$(NORMAL)"
	@ echo "    You can run jarlang with './$(DEBUGDIR)/jarlang.sh FILE1 FILE2 FILE3...'"
	@ echo "    Done\n"
test:
	@ echo "$(PURPLE)==> Building TEST$(NORMAL)"
	@ echo "    Compiling giles with debug_info enabled"
	@ echo "    Compiling files with warnings ignored"
	@ echo "    Compiling giles will fail if any errors occur"
	@ mkdir -p $(TESTDIR)
	@ rm -f $(TESTDIR)/*.beam
	@ rm -f $(TESTDIR)/*.sh
	@ rm -f $(TESTDIR)/*.js
	@ echo "$(PURPLE)==> Compiling Library Files$(RED)"
	@ $(ERLC) $(DEBUGFLAGS) $(TESTDIR) $(LIBDIR)/*.erl
	@ echo "$(NORMAL)    Done"
	@ echo "$(PURPLE)==> Compiling Source Files$(RED)"
	@ $(ERLC) $(DEBUGFLAGS) $(TESTDIR) $(SRCDIR)/*.erl
	@ echo "$(NORMAL)    Done"
	@ echo "$(PURPLE)==> Bootstrapping Source Files onto build$(RED)"
	@ cp $(SRCDIR)/*.erl $(TESTDIR) 2> /dev/null || true
	@ cp $(LIBDIR)/*.erl $(TESTDIR) 2> /dev/null || true
	@ echo "$(NORMAL)    Done"
	@ echo "$(PURPLE)==> Bootstrapping NodeJS Environment onto build$(RED)"
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(SRCDIR)/*.js $(TESTDIR) 2> /dev/null || true
	@ $(JSMINIFY) $(JSMINIFYFLAGS) $(LIBDIR)/*.js $(TESTDIR) 2> /dev/null || true
	@ echo "$(NORMAL)    Done"
	@ echo "$(PURPLE)==> Creating test script in './$(TESTDIR)/'$(NORMAL)"
	@ echo "$(NORMAL)    Done"
	@ cp -f $(MISCDIR)/* $(TESTDIR) 2> /dev/null || true
	@ echo "-mode eunit" >> $(TESTDIR)/jarlang.sh && mv $(TESTDIR)/jarlang.sh $(TESTDIR)/run_tests.sh
	@ echo "$(PURPLE)==> Running EUnit tests and XREF analyses$(NORMAL)"
	@ ./$(TESTDIR)/run_tests.sh
	@ echo "$(PURPLE)==> Running Dialyzer$(NORMAL)"
	@ dialyzer $(TESTDIR)/*.beam || true
	@ echo "$(PURPLE)==> Finished Testing, results are printed to console$(NORMAL)"
	@ echo "    You can re-run tests for this build with './$(TESTDIR)/run_tests.sh'"
	@ echo "    Done\n"
clean:
	@ echo "$(ORANGE)==> Cleaning builds"
	@ find . -name "*.beam" -delete
	@ echo "==> Removing all BEAM files from workspace"
	@ find . -name "*.dump" -delete
	@ echo "==> Removing all DUMP files from workspace"
	@ rm -rf $(OUTDIR)
	@ echo "==> Removing $(OUTDIR)/"
	@ rm -rf $(DEBUGDIR)
	@ echo "==> Removing $(DEBUGDIR)/"
	@ rm -rf $(TESTDIR)
	@ echo "==> Removing $(TESTDIR)/"
	@ echo "==> Cleaned\n$(NORMAL)"
