# sicp-interpreter

A mostly-complete implementation of the interpreters from sections 4.1 and 4.2 of
[Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/index.html).

The original interpreter (section 4.1) is in the `master` branch, and the lazy evaluator of
section 4.2 in in the `lazy` branch.

I implemented it in Racket and experimented with a few things I hadn't used before (e.g. Racket's classes and objects).
It doesn't implement everything covered in the book, in particular `quote` is missing.
