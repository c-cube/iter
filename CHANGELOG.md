# Changelog


## 0.9

- distinction between `diagonal,diagonal_l`
- add `init,fold_map,fold_filter_map,sorted,diagonal,findi,…`
- fix a few typos
- update readme: convert into asciidoc, add tutorial
- remove deprecated functions, add missing links to `SequenceLabels`

## 0.8

- loop based implementation for `int_range_by`
- move files to 'src/', use qtest for tests
- add `int_range_by`
- add `Sequence.flat_map_l`

## 0.7

- add missing entry in changelog and missing since annotations
- Add `shuffle`.
- Add `shuffle_buffer`.
- Add `sample`.
- Add `map_by_2`.

## 0.6

- deprecate `flatMap` and `fmap`
- in opam file, depend on ocamlbuild
- add `group_by` (ignores the ordering)
- alias `group_succ_by`, deprecated `group`
- iterate on booleans
- open Bigarray (preparing for 4.03)

## 0.5.5

- new module `SequenceLabels`
- `fold_while` fun
- implement `Set.Adapt.of_list` for `< 4.02`
- removed many warnings, fix tests
- change name of `IO` functions (keep compat)

## 0.5.4

- depend on `bytes`
- compliance with `-safe-string`
- `sequence.bigarray`

## 0.5.3

- bugfix: interaction between `take` and `is_empty`

## 0.5.2

- bugfix in `take`
- `on_list` for mapping lists through sequences

## 0.5.1

- `Sequence.IO` module, a very very simple way to read/write files
- options: `to_opt/of_opt/head/head_exn`

## 0.5

- conversion with `klist`
- add monadic, choice and applicative infix operators and `>|=`
- add several functions:
  * `product2`
  * `find`, `mem`
  * `doubleton`, `cons`, `snoc`
  * `drop_while`, `take_while`...
  * `concat_str`
- aliases to existing functions
- use `delimcc` in a new module, `SequenceInvert`, in order to reverse the
  control flow (here with conversion to Gen)
- fix examples, tests and doc (about `product`)
- reading benchmark for persistent sequences.
- replace `Bench` with `Benchmark`

## 0.4.1

- `persistent_lazy`
- use `bin_annot`

## 0.4

- API change for `persistent`
- more efficient implementation for `persistent`
- remove `TypeClass`
- API change for `min`/`max` (in case the sequence is empty)
- conversion with `Gen`
- use Oasis

## 0.3.7

- decreasing int range
- printing functions

## 0.3.6.1

- documentation
- bugfixes

## 0.3.6

- `fmap`
- functors to adapt `Set` and `Map`

## 0.3.5

- tests and benchmarks
- `join` combinator
- optimization for `Sequence.persistent`

## 0.3.4

- `sort`, `uniq`, `group` and `sort_uniq` combinators implemented
- some conversion functions that use `Sequence.t2`
- infix operators in `Sequence.Infix`
- `Sequence.t2` type for efficient iteration on pairs of elements
- some combinators are adapted to `Sequence.t2`
- `zip`, `unzip` and `zip_i` to convert between `t` and `t2`
- added `scan` combinator

note: `git log --no-merges previous_version..HEAD --pretty=%s`
