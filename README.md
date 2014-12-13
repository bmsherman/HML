# Getting started with HML

## Introduction

HML is a toy functional programming language inspired by ML
(as well as Haskell). Its properties and features include:

 * a Hindley-Milner-like type system including
   - Hindley-Milner type inference (type annotations are never necessary)
   - existential quantification
   - optional type annotations almost anywhere
 * prenex polymorphism of functions and datatypes
 * algebraic data types
 * simple pattern matching
 * strict evaluation
 * impure functions including I/O and array operations
 * compilation to x64 assembly (GAS) with
   - direct compatibility with System V AMD64 ABI
   - tail call optimization

Some notable features that HML lacks are:
  * closures (/ nested function definitions)
    - but existential quantification gives DIY closures
  * currying (since closures aren't built-in)
  * garbage collection 
    - it makes plenty of garbage! it just doesn't clean up

Let's get to the "Hello, World":

```HML
main() = out_string("Hello, World!");
```

Saving this file to `hello.hm`, we can compile and run our program 
with the following commands:
```bash
cat hello.hm | HMCompile > hello.S       # generate x64 assembly
gcc hello.S -o hello                     # assemble and link
./hello                                  # run!
```

Additional code examples are available in the `sample-src` directory.

# Types

The syntax of type expressions looks like this:

```
  t, u, v, w ...            -- types
  a                         -- type variables
  F                         -- type constructors

  types t ::=
      Int                   -- 64-bit integer primitive type
    | String                -- opaque String primitive type
    | a                     -- type variable
    | F(u, v, ...)          -- application of type constructor
    | (u, v, ...) -> w      -- function type


  (ellipsis indicate any natural number of arguments)

```

## Data type declarations

There are two sorts of top-level declarations that can be made in HML:
data type definitions and function definitions.

Here's data type definition where we define booleans:
```HML
data Bool() of True() | False();
```

Booleans are actually built-in, so this declaration can be found in the 
Prelude (`Prelude.hm`), whose declarations are automatically inserted before
reading any source file.

We call `Bool` a type constructor and `True` and `False` data constructors.
Effectively, `Bool` is a nullary function on types, and `True` and `False`
are nullary functions on terms. Names for type constructors and data
constructors must begin with uppercase variables.

There are two primitive types: `Int`, which represents 64-bit signed integers,
and `String`, which represents (opaque) strings of ASCII characters. We can
recursively define a linked list of integers like this:
```HML
data IntList() of Nil() | Cons(Int, IntList());
```

Type variables allow polymorphism. Consider a definition of lists polymorphic
in the type of their elements:
```HML
data List(a) of Nil() | Cons(a, List(a));
```

Type variables must begin with lowercase letters. Their scope is local to the
top-level declaration in which they are used. For data declarations, simply
using a type variable as an argument to a type or data constructor introduces
it into scope.

Using a type variable in a data constructor, but not in the corresponding
type constructor, results in existential quantification. For instance, let's
say that we wish to make a datatype representing arguments of two variables
for which the first argument has already been applied:
```HML
data F(a2, b) of F(a1, (a1, a2) -> b);
```

(Note that we can use the same name for a type constructor and data constructor
, since terms and types are completely different syntactic categories.)
It might help to imagine this data declaration as creating a function
declaration that looks like this:
```HML
F(x : a1, f : (a1, a2) -> b) : F(a2, b) = #magic#;
```

Here, we see that the function F is *universally* quantified over the type
variable 'a1'. That means that whenever we inspect a term with type
`F(a2, b)`, we don't know the type 'a1', and so we must work with it in a
polymorphic manner.

# Terms

The term expression language looks like this:

```
  e1, e2, e3            -- expressions
  v1, v2, v3            -- variables
  t                     -- type variables
  p1, p2, p3            -- productions
  F, G, H               -- data constructors


expressions e ::=
    13                            -- integer literal
  | "Hello\nWorld"                -- string literal
  | v1(e1, e2, ...)               -- function application
  | let v1 = e1 in e2             -- let statement
  | case e1 { p1 | p2 | ... }     -- case expression
  | e : t                         -- explicit type annotation

productions p ::=
    F(v1, v2, ...) => e            

```

## Pattern matching
Only simple pattern matching is allowed. That is, catch-all patterns like this:
```HML
case True() { v => v };
```
are not allowed, and neither are nested patterns like this:
```HML
case And(And(1,2), 3) { And(And(x, y), z) => x + y + z };
```


## Evaluation order
Since HML is strict and impure, evaluation order matters. In the case of
function application, arguments are evaluated from left to right. In the
let assignment `let v = e1 in e2`, `e1` is evaluated before `e2`.
The sequencing operator `>>`, is simply syntactic sugar:
```HML
e1 >> e2         ==>      seq(e1, e2)

seq(x, y) = y
```

## Explicit type annotations
Explicit type annotations are never necessary, but they can be of assistance
both in developing as well as documenting code. Type variables may be
introduced into the scope of a function declaration simply by using them
inside type annotations for the arguments or the return type. For example,
we can write
```HML
seq(x : a, y : b) : b = y;
```

These type variables are then in scope in the body of the function definition.
Type variables cannot be introduced into the scope in any other way.
For example, the following is invalid, because the type variable 'a' is not
in scope:
```HML
myFunction() = Nil() : List(a);
```
even though this is perfectly acceptable:
```HML
myFunction() : List(a) = Nil() : List(a);
```

Explicit type annotations can allow you to make a function's type more
restrictive than type inference dictates. For example, we can specialize
the `seq` function to operate only on integers:
```HML
seqInt(x : Int, y : Int) = seq(x, y);
```
