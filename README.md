# How to run the tests

First, ensure that you have installed the Haskell Platform.

Then, run `HW4.sh`. This script compiles all of the source code in
the `sample-src/uncool_tests/` directory (which correspond to the UnCool
sample code files for Lab 4) as well as several example source files
found directly in the `sample-src/` directory. The compiled code is saved
as .S files which are compatible with the GNU Assembler. It then packages
these up, along with a Makefile, into the file `Ben-Sherman-asm.tar.gz`.

Copy the `Ben-Sherman-asm.tar.gz` file over to Zeus. Then, decompress
the file and enter the `sample-asm/` that was just created. Run
`make` to assemble all of the assembly files into executables. The
executables will be put into the `sample-asm/bin/` directory. You may
then run the executables.

NOTE: The executables `Stream` and `FactStream`, as the name suggests,
will never voluntarily stop. When you're tired of seeing numbers flash
across the screen, you can interrupt and kill the program with Ctrl-C.

Some more information about the language and its semantics can be found
in the `GettingStarted.markdown` file.
