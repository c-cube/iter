Sequence
========

Simple sequence abstract datatype, intented to transfer a finite number of
elements from one data structure to another. It also provides a tiny
library for S-expressions, convertible to streams of tokens, and conversely.

Build
=====

You need OCaml, say OCaml 3.12 or OCaml 4.0.

    $ make

To see how to use it, check `tests.ml`. `sequence.ml` has a few examples of how to convert
data structures into sequences, and conversely. The module `sexpr.mli` exposes the interface
of the S-expression library.

License
=======

Sequence is available under the BSD license.
