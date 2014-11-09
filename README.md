------------ How to run the tests -------------

First, ensure that you have installed the Haskell Platform.

Then, run `run.sh`. This script runs the typechecker over all source code
files found in the `sample-src` directory.

----- More detailed notes on typechecking -----

Source code written in this programming language is given the ".hm"
file extension. The file `Prelude.hm` contains primitive, magical,
or otherwise necessary functions. These functions are typechecked
and inserted into the typing context before actual source code is
typechecked.

The files in the `sample-src` directory are all source code for this
programming language. The directory `sample-src/error_files` contains
source files which have errors that should be found during typechecking.
This includes a variety of errors:
  - Scope errors for both term variables and type variables
  - Type errors for terms, and kind errors for explicitly given types
  - Error for duplicate function arguments

A notable difference between the semantics of UnCool and of my programming
language is that name shadowing is allowed for my programming language.
That is, I can validly write
```
let x = 3 in (let x = True() in not(x))
```
, where the reference to 'x' as the argument to the function 'not' refers
to the 'x' that is bound most closely ('True()') in this case).

The files that are directly in the `sample-src` directory are valid
programs that should not have any typechecking errors. I do not provide
many types in these source files, so they serve to show off type inference.

The typechecker reads source files from standard input. It then outputs
its results to standard output. First, it outputs a list of errors that
it encountered while reading the source code. Then, it prints all type
constructors along with their kinds. Finally, it prints all global
functions which succesfully typechecked along with their types.

Concrete types begin with uppercase letters, while type variables begin
with lowercase letters. Type variables in the type signatures that are
output by the typechecker are (implicitly) universally quantified over.

Typechecking proceeds incrementally. A datatype or function is not in scope
until "after" it is defined; that is, it's only in scope in the part of the
file which comes after/below the definition.

How type variable scoping works: In a datatype declaration, type variables
are introduced into scope by being written as arguments to the type
constructor. These are the only type variables in scope for the 
data constructor definitions. In a function declaration, type variables
may be introduced into scope by placing them anywhere within
type expressions for the argument types or the return type. These type
variables are then in scope for all typing declarations within the
function's body. All type variables are (implicitly) universally
quantified over in the function's type. They are obviously not
quantified over in typing declarations in the body, which corresponds with
the global scoping rules.
