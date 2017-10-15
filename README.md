This repos contains pattern match exhaustiveness checking and compilation
to decision trees a al Maranget.

`Checker.hs` implements pattern exhaustiveness checking according to the
following paper: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.530.8651&rep=rep1&type=pdf

`Compiler.hs` implements compilation of patterns to decision trees
according to the following paper: http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf

Both of these algorithms share a number of algorithmic similarities, and
these shared algorithms are implemented in `Common.hs`

