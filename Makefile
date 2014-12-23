rootdir := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
cwd := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

eggs = readline/trunk

all: build

build:
	find /usr/bin -name 'gcc*' -print
	/usr/bin/gcc --version
	$(foreach dir,$(eggs),cd $(dir); make -B)

clean:
	$(foreach dir,$(eggs),cd $(dir); make clean -B)

dist-clean:
	$(foreach dir,$(eggs),cd $(dir); make dist-clean -B)
