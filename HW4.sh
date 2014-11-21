# Author: Ben Sherman
# Language: Haskell

# Instructions:
# Simply run this shell script. The first two commands compile the
# Haskell program, and the rest of the script runs the compiler on
# several test files, outputting assembly files into the package
# `sample-asm.tar.gz`

# compile
cabal configure
cabal build

# run

mkdir -p sample-asm/uncool_tests/

for i in `seq 0 7`
do
  file=sample-src/uncool_tests/test${i}.hm
  cat $file | dist/build/Compile/Compile > sample-asm/uncool_tests/test${i}.S
done

for f in ArrayExample BST Error Fact Mergesort Quicksort Stream
do
  cat sample-src/${f}.hm | dist/build/Compile/Compile > sample-asm/${f}.S
done

tar -zcvf Ben-Sherman-asm.tar.gz sample-asm/
