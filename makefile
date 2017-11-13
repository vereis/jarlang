SHELL = /bin/sh

# Compilation Variables
ERLC = $(shell which erlc)
ERLFLAGS = -Werror -v -o
DEBUGFLAGS = -W0 -o
JSMINIFY = cp
JSMINIFYFLAGS = -f

# Directory Variables
SRCDIR = src
LIBDIR = src/lib
OUTDIR = ebin
DEBUGDIR = edebug
MISCDIR = misc

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
	@ echo "$(GREEN)    Treating all warnings as errors. Build will fail if any errors occur$(NORMAL)"
	@ mkdir -p $(OUTDIR)
	@ rm -f $(OUTDIR)/*.beam
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
	@ echo "$(GREEN)==> Creating launch script './$(OUTDIR)/'$(NORMAL)"
	@ cp -f $(MISCDIR)/* $(OUTDIR) 2> /dev/null || true
	@ echo "$(NORMAL)    Done"
	@ echo "$(GREEN)==> RELEASE release successfully built in './$(OUTDIR)/'$(NORMAL)"
	@ echo "    You can run jarlang with './$(OUTDIR)/jarlang.sh FILE1 FILE2 FILE3...'"
	@ echo "    Done\n"
debug:
	@ echo "$(BLUE)==> Building DEBUG$(NORMAL)"
	@ echo "    Build will fail if any errors occur. Warnings are ignored"
	@ mkdir -p $(DEBUGDIR)
	@ rm -f $(DEBUGDIR)/*.beam
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
	@ echo "$(BLUE)==> Creating launch script './$(DEBUGDIR)/'$(NORMAL)"
	@ cp -f $(MISCDIR)/* $(DEBUGDIR) 2> /dev/null || true
	@ echo "$(NORMAL)    Done"
	@ echo "$(BLUE)==> DEBUG release successfully built in './$(DEBUGDIR)/'$(NORMAL)"
	@ echo "    You can run jarlang with './$(DEBUGDIR)/jarlang.sh FILE1 FILE2 FILE3...'"
	@ echo "    Done\n"
clean:
	@ echo "$(ORANGE)==> Cleaning builds"
	@ rm -rf $(OUTDIR)
	@ echo "==> Removing out/"
	@ rm -rf $(DEBUGDIR)
	@ echo "==> Removing debug/"
	@ echo "==> Cleaned\n$(NORMAL)"