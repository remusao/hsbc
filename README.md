# hsbc

Haskell challenge to create an arithmetic expressions evaluator, from scratch.

## Goals

The goal of this project is to implement a basic (definition of basic can evolve over time), arithmetic expression evaluation command written in *Haskell*.
It should first return *correct results*, and then try to *be as fast as possible* (just for fun). The reference program that one can use is `bc`.

The following operators should be supported:

* Integers of arbitrary size.
* Addition and substraction.
* Division and multiplication.
* Groupping of expressions with parenthesis.

## Constraints

This problem should be solved in Haskell, with GHC >= 8.0.1, and **no external library is allowed** appart from the standard Prelude.

## Evaluation

Some basic test cases are provided in the `tests.txt` file, and you can run tests this way:

```sh
$ ./eval.sh <you_program>
```

For example:

```sh
$ ./eval.sh bc
............
real	0m0.049s
user	0m0.000s
sys	0m0.008s
```

The file has the following format. Each test takes two lines, one expression, and the expected result. For example:

```sh
1 + 1
2
```
