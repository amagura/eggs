rootdir := $(abspath $(lastword $(MAKEFILE_LIST)))
cwd := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
startdir := "$$HOME/code/readline/extra"
CHK_CC := /usr/bin/chicken-install
version := $$(grep version "${startdir}/../trunk/readline.setup" | grep -P '[0-9.]+' -o)

all: kill build

build:
	cd ${startdir}; \
	  cd ../trunk ;\
	  sudo $(CHK_CC) -trunk
	  touch build

kill:
	killall -9 csi || true

check: kill build
	cd ${startdir}; \
	  cd ../trunk; \
	  csi -n tests/run.scm
	cd ${startdir}; \
	  ./test.bats

sure: kill
	cd ${startdir}; \
	  cd ../trunk; \
	  salmonella

clean: tidy
	cd ${startdir}; \
	  $(RM) build; \
	  $(RM) test; \
	  cd ../trunk ;\
	  for _f in *.so *.html; do $(RM) "$$_f"; done

tidy:
	cd ${startdir}; \
	  cd ../trunk; \
	  for _f in *.{o,out} build temp* readline.c readline.import.scm readline-test *.log; do $(RM) "$$_f"; done

# XXX NOT for installing this egg.  This installs the makefile into the other directories of this egg.
install:
	cd ${startdir}; \
	  cd ../trunk; \
	  ln -s "${startdir}/Makefile"


release: sure doc clean
	cd ${startdir}; \
	  cd ..; \
	  svn copy trunk tags/$(version) ;\
	  cd tags/$(version) ;\
	  $(RM) Makefile
test:


doc:
	#cd ${startdir}; \
	#  cd ../trunk; \
	#  for _f in $(DOCS); do asciidoc --theme volnitsky "$$_f"; done
