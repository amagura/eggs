#  Copyright 2015 Alexej Magura
#
#  Copying and distribution of this file, with or without modification,
#  are permitted in any medium without royalty provided the copyright
#  notice and this notice are preserved.  This file is offered as-is,
#  without any warranty.

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mydir := $(abspath $(lastword $(dir $(MAKEFILE_LIST))))
cwd := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

C_SRC = $(filter %-egg.c, $(wildcard $(mydir)/../trunk/*.c)) common.c common.h

splint := /usr/bin/env splint
cppcheck := /usr/bin/env cppcheck
valgrind := /usr/bin/env valgrind
VALGRIND = --show-leak-kinds=all --leak-check=full --
LINT = -nullassign -immediatetrans +trytorecover +gnuextensions +posixlib
CHK_CC := /usr/bin/chicken-install
version := $$(grep version "$(mydir)/../trunk/readline.setup" | grep -P '[0-9.abc]+' -o)

.PHONY: build kill
all: kill build

valgrind:
	$(valgrind) $(VALGRIND) csi -n $(mydir)/../trunk/tests/run.scm &> $(mydir)/../trunk/valgrind

lint:
	$(foreach f,$(C_SRC), $(cppcheck) --enable=all $(f);)
	$(foreach f,$(C_SRC), $(splint) $(LINT) $(f) || true;) # splint always fails

build:
	cd $(mydir)/../trunk; \
	  sudo CFLAGS="$(CFLAGS)" $(CHK_CC) -trunk

kill:
	killall -9 csi || true

check: kill build
	cd $(mydir)/../trunk; \
	  csi -n tests/run.scm
	cd $(mydir)/../extra; \
	  STARTDIR=. ./test.bats;

sure: kill
	cd $(mydir)/../trunk; \
	  salmonella

clean: tidy
	cd $(mydir); \
	  $(RM) build; \
	  $(RM) test; \
	  $(RM) units; \
	  cd ../trunk ;\
	  $(RM) common.o ;\
	  for _f in *.so *.html; do $(RM) "$$_f"; done; \
	  $(RM) valgrind

tidy:
	cd $(mydir); \
	  cd ../trunk; \
	  for _f in *.{o,out} build temp* readline.c readline.import.scm readline-test *.log; do $(RM) "$$_f"; done

# XXX NOT for installing this egg.  This installs the makefile into the other directories of this egg.
install:
	cd $(mydir); \
	  cd ../trunk; \
	  ln -s "$(mydir)/../extra/Makefile"


release: sure doc clean
	cd $(mydir); \
	  cd ..; \
	  svn copy trunk tags/$(version) ;\
	  cd tags/$(version) ;\
	  $(RM) Makefile

test-release: sure doc
	cd $(mydir); \
	  cd ..; \
	  echo $(version)

dist-clean: clean
	cd $(mydir)/../tags; \
	  $(foreach ext,'*.so' '*.o' '*.out' '*.log' 'readline.c' 'readline.import.scm' 'readline-test' 'temp*',find ./ -name $(ext) -delete)

doc:
	#cd $(mydir); \
	#  cd ../trunk; \
	#  for _f in $(DOCS); do asciidoc --theme volnitsky "$$_f"; done

% : %.c
	$(CC) $(CFLAGS) -ggdb -g $(LDFLAGS) -lreadline -o $@ $<

%.gpp : %.g
	cd $(mydir)/../extra; \
	cat macros.gpp $< > $@

%.c : %.gpp
	cd $(mydir)/../extra; \
	gpp --nostdinc -O $@ $<
	$(RM) $<

expand:
	$(CC) $(CFLAGS) -ggdb -g -lreadline -E -o readline-egg.e readline-egg.c
