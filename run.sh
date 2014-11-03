# Author: Ben Sherman
# Language: Haskell

# Instructions:
# Simply run this shell script. The first two commands compile the
# Haskell program, and the rest of the script runs the typechecker on
# several test files.

# compile
cabal configure
cabal build

# run

for f in sample-src/error_files/*.hm sample-src/*.hm
do
  echo "File: $f"
  echo "--------"
  cat $f
  echo "--------"
  cat $f | dist/build/Typecheck/Typecheck
  echo -e "--------\n--------\n\n"
done
