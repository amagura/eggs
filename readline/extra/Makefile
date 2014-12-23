rootdir := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
cwd := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
CHK_CC := /usr/bin/chicken-install
version := $$(grep version "${rootdir}/../trunk/readline.setup" | grep -P '[0-9.]+' -o)

all: kill build

build:
	cd ${rootdir}/../trunk; \
	  sudo $(CHK_CC) -trunk
	  touch build

kill:
	killall -9 csi || true

check: kill build
	cd ${rootdir}/../trunk; \
	  csi -n tests/run.scm
	cd ${rootdir}/../extra; \
	  ./test.bats

sure: kill
	cd ${rootdir}/../trunk; \
	  salmonella

clean: tidy
	cd ${rootdir}; \
	  $(RM) build; \
	  $(RM) test; \
	  cd ../trunk ;\
	  for _f in *.so *.html; do $(RM) "$$_f"; done

tidy:
	cd ${rootdir}; \
	  cd ../trunk; \
	  for _f in *.{o,out} build temp* readline.c readline.import.scm readline-test *.log; do $(RM) "$$_f"; done

# XXX NOT for installing this egg.  This installs the makefile into the other directories of this egg.
install:
	cd ${rootdir}; \
	  cd ../trunk; \
	  ln -s "${rootdir}/../extra/Makefile"


release: sure doc clean
	cd ${rootdir}; \
	  cd ..; \
	  svn copy trunk tags/$(version) ;\
	  cd tags/$(version) ;\
	  $(RM) Makefile
test-release: sure doc
	cd ${rootdir}; \
	  cd ..; \
	  echo $(version)

dist-clean: clean
	cd ${rootdir}/../tags; \
	  $(foreach ext,'*.so' '*.o' '*.out' '*.log' 'readline.c' 'readline.import.scm' 'readline-test' 'temp*',find ./ -name $(ext) -delete)

doc:
	#cd ${rootdir}; \
	#  cd ../trunk; \
	#  for _f in $(DOCS); do asciidoc --theme volnitsky "$$_f"; done
