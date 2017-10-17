SHELL = /bin/sh

# Compilation Variables
ERLC = $(shell which erlc)
ERLFLAGS = -Werror -v -o
DEBUGFLAGS = -o

# Directory Variables
SRCDIR = src
LIBDIR = lib
OUTDIR = ebin
DEBUGDIR = edebug

release:
	@ echo "-- Building release build : Will fail if there are any warnings or errors" ;
	@ mkdir -p $(OUTDIR) ;
	@ $(ERLC) $(ERLFLAGS) $(OUTDIR) $(LIBDIR)/*.erl ;
	@ $(ERLC) $(ERLFLAGS) $(OUTDIR) $(SRCDIR)/*.erl ;
	@ echo "Built Release in './$(OUTDIR)/'.\n" ;
debug:
	@ echo "-- Building debug build : Will fail if there are any errors" ;
	@ mkdir -p $(DEBUGDIR) ;
	@ $(ERLC) $(DEBUGFLAGS) $(DEBUGDIR) $(LIBDIR)/*.erl ;
	@ $(ERLC) $(DEBUGFLAGS) $(DEBUGDIR) $(SRCDIR)/*.erl ;
	@ echo "Built Debug in './$(DEBUGDIR)/'.\n" ;
clean:
	@ echo "-- Cleaning builds" ;
	@ rm -rf $(OUTDIR) ;
	@ echo "Removing out/" ;
	@ rm -rf $(DEBUGDIR) ;
	@ echo "Removing debug/" ;
	@ rm -rf erl_crush.dump ;
	@ echo "Removing crash dump" ;
	@ echo "Cleaned.\n" ;
