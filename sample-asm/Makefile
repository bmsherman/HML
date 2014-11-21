ASM_FILES := $(wildcard */*.S) $(wildcard *.S)
EXECUTABLES := $(addprefix bin/,$(ASM_FILES:.S=))


all: $(EXECUTABLES)

bin/uncool_tests/% : uncool_tests/%.S
	mkdir -p bin/uncool_tests/
	gcc $< -o $@

bin/% : %.S
	mkdir -p bin/
	gcc $< -o $@

clean:
	rm -r bin/

