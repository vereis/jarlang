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

# Misc
LOGMSG = echo "If build fails, log message will be stored in ./buildlog.log"
LOGHEADER = echo -- Build started on $(shell date) --

release:
	@ echo "-- Building release build : Will fail if there are any warnings or errors" ;
	@ $(LOGMSG) ;
	@ $(LOGHEADER) >> ./buildlog.log;
	@ mkdir -p $(OUTDIR) ;
	@ rm $(OUTDIR)/*.beam >> /dev/null
	@ $(ERLC) $(ERLFLAGS) $(OUTDIR) $(LIBDIR)/*.erl >> ./buildlog.log ;
	@ $(ERLC) $(ERLFLAGS) $(OUTDIR) $(SRCDIR)/*.erl >> ./buildlog.log ;
	@ rm ./buildlog.log ;
	@ echo "Built Release in './$(OUTDIR)/'.\n" ;
prepush:
	@ mkdir -p $(OUTDIR) ;
	@ rm $(OUTDIR)/*.beam >> /dev/null
	@ $(LOGMSG) ;
	@ $(LOGHEADER) >> ./buildlog.log;
	@ $(ERLC) $(ERLFLAGS) $(OUTDIR) $(LIBDIR)/*.erl >> ./buildlog.log ;
	@ $(ERLC) $(ERLFLAGS) $(OUTDIR) $(SRCDIR)/*.erl >> ./buildlog.log ;
	@ rm ./buildlog.log ;
debug:
	@ echo "-- Building debug build : Will fail if there are any errors" ;
	@ $(LOGMSG) ;
	@ $(LOGHEADER) >> ./buildlog.log;
	@ mkdir -p $(DEBUGDIR) ;
	@ rm $(DEBUGDIR)/*.beam >> /dev/null
	@ $(ERLC) $(DEBUGFLAGS) $(DEBUGDIR) $(LIBDIR)/*.erl >> ./buildlog.log ;
	@ $(ERLC) $(DEBUGFLAGS) $(DEBUGDIR) $(SRCDIR)/*.erl >> ./buildlog.log ;
	@ rm ./buildlog.log ;
	@ echo "Built Debug in './$(DEBUGDIR)/'.\n" ;
clean:
	@ echo "-- Cleaning builds" ;
	@ rm -rf $(OUTDIR) ;
	@ echo "Removing out/" ;
	@ rm -rf $(DEBUGDIR) ;
	@ echo "Removing debug/" ;
	@ rm -rf erl_crush.dump ;
	@ echo "Removing crash dump" ;
	@ touch ./buildlog.log && rm ./buildlog.log ;
	@ echo "Cleaned.\n" ;
