Sequence
========

Simple sequence abstract datatype, intented to transfer a finite number of
elements from one data structure to another. Some transformations on sequences,
like `filter`, `map`, `take`, `drop` and `append` can be performed before the
sequence is iterated/folded on.

Sequence is not designed to be as general-purpose or flexible as, say,
Batteries' `Enum.t`. Rather, it aims at providing a very simple and efficient
way of iterating on a finite number of values, only allocating (most of the time)
one intermediate closure to do so. For instance, iterating on keys, or values,
of a `Hashtbl.t`, without creating a list.

Build
=====

You need OCaml, say OCaml 3.12 or OCaml 4.0.

    $ make

To see how to use it, check `tests.ml`. `sequence.ml` has a few examples of how to convert
data structures into sequences, and conversely.

The module `sexpr.mli` exposes the interface of the S-expression example library. It
requires OCaml>=4.0 to compile, because of the GADT structure used in the monadic
parser combinators part of `sexpr.ml`.

License
=======

Sequence is available under the BSD license.
