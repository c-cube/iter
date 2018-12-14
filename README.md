# Iter

Simple abstraction over `iter` functions, intended to iterate efficiently
on collections while performing some transformations. Used to be called `Sequence`.

Common operations supported by `Iter` include
`filter`, `map`, `take`, `drop`, `append`, `flat_map`, etc.
`Iter` is not designed to be as general-purpose or flexible as, say,
Batteries' `'a Enum.t`. Rather, it aims at providing a very simple and efficient
way of iterating on a finite number of values, only allocating (most of the time)
one intermediate closure to do so. For instance, iterating on keys, or values,
of a `Hashtbl.t`, without creating a list.

[![build status](https://travis-ci.org/c-cube/iter.svg?branch=master)](https://travis-ci.org/c-cube/iter)

## Documentation

There is only one important type, `'a Iter.t`, and lots of functions built
around this type.
To get an overview of iter (originally "sequence"), its origins and why it was created,
you can start with [the slides of a talk](http://simon.cedeela.fr/assets/talks/sequence.pdf)
I (@c-cube) made at some OCaml meeting.

See [the online API](https://c-cube.github.io/iter/)
for more details on the set of available functions.

## Build

1. via opam `opam install iter`
2. manually (need OCaml >= 4.02.0): `make all install`

If you have [qtest](https://github.com/vincent-hugot/qtest) installed,
you can build and run tests with

```
$ make test
```

If you have [benchmarks](https://github.com/Chris00/ocaml-benchmark) installed,
you can build and run benchmarks with

```
$ make benchs
```

To see how to use the library, check the following tutorial.
The `tests` and `examples` directories also have some examples, but they're a bit arcane.

## Short Tutorial

### Transferring Data

Conversion between n container types
would take n² functions. In practice, for a given collection
we can at best hope for `to_list` and `of_list`.
With iter, if the source structure provides a
`iter` function (or a `to_iter` wrapper), it becomes:

```ocaml
# #use "src/Iter.ml";;
# let q = Queue.create();;
# Iter.( 1 -- 10 |> to_queue q);;
- : unit = ()
# Iter.of_queue q |> Iter.to_list ;;
- : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

# let s = Stack.create();;
# Iter.(of_queue q |> to_stack s);;
- : unit = ()
# Iter.of_stack s |> Iter.to_list ;;
- : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1] 
```

Note how the list of elements is reversed when we transfer them
from the queue to the stack.

Another example is extracting the list of values of
a hashtable (in an undefined order that depends on the
underlying hash function):

```ocaml
# let h = Hashtbl.create 16;;
# for i = 0 to 10 do
     Hashtbl.add h i (string_of_int i)
  done;;
- : unit = ()

# Hashtbl.length h;;
- : int = 11

(* now to get the values *)
# Iter.of_hashtbl h |> Iter.map snd |> Iter.to_list;;
- : string list = ["6"; "2"; "8"; "7"; "3"; "5"; "4"; "9"; "0"; "10"; "1"] 
```

### Replacing `for` loops

The `for` loop is a bit limited, and lacks compositionality.
Instead, it can be more convenient and readable to
use `Iter.(--) : int -> int -> int Iter.t`.

```ocaml
# Iter.(1 -- 10_000_000 |> fold (+) 0);;
- : int = 50000005000000

# let p x = x mod 5 = 0 in
  Iter.(1 -- 5_000
    |> filter p
    |> map (fun x -> x * x)
    |> fold (+) 0
  );;
- : int = 8345837500
```

**NOTE**: with _flambda_ under sufficiently strong
optimization flags, such compositions of operators
should be compiled to an actual loop with no overhead!

### Iterating on sub-trees

A small λ-calculus AST, and some operations on it.

```ocaml
# type term =
  | Var of string
  | App of term * term
  | Lambda of term ;;

# let rec subterms : term -> term Iter.t =
  fun t ->
    let open Iter.Infix in
    Iter.cons t
      (match t with
      | Var _ -> Iter.empty
      | Lambda u -> subterms u
      | App (a,b) ->
        Iter.append (subterms a) (subterms b))
  ;;
  
(* Now we can define many other functions easily! *)
# let vars t =
    Iter.filter_map
      (function Var s -> Some s | _ -> None)
      (subterms t) ;;
val vars : term -> string sequence = <fun >

# let size t = Iter.length (subterms t) ;;
val size : term -> int = <fun >

# let vars_list l = Iter.(of_list l |> flat_map vars);;
val vars_list : term list -> string sequence = <fun >
```

### Permutations

Makes it easy to write backtracking code (a non-deterministic
function returning several `'a`
will just return a `'a Iter.t`).
Here, we generate all permutations of a list by
enumerating the ways we can insert an element in a list.

```ocaml
# open Iter.Infix;;
# module S = Iter ;;
# let rec insert x l = match l with
  | [] -> S.return [x]
  | y :: tl ->
    S.append
      S.(insert x tl >|= fun tl' -> y :: tl')
      (S.return (x :: l)) ;;

# let rec permute l = match l with
  | [] -> S.return []
  | x :: tl -> permute tl >>= insert x ;;

# permute [1;2;3;4] |> S.take 2 |> S.to_list ;;
- : int list list = [[4; 3; 2; 1]; [4; 3; 1; 2]]
```

### Advanced example

The module `examples/sexpr.mli` exposes the interface of the S-expression
example library. It requires OCaml>=4.0 to compile, because of the GADT
structure used in the monadic parser combinators part of `examples/sexpr.ml`.
Be careful that this is quite obscure.

## Comparison with [gen](https://github.com/c-cube/gen)

- `Gen` is an *external* iterator.
  It means that the code which consumes
  some iterator of type `'a Gen.t` is the one which decides when to
  go to the next element. This gives a lot of flexibility, for example
  when iterating on several iterators at the same time:

  ```ocaml
  let zip (g1: 'a Gen.t) (g2:'b Gen.t) : ('a * 'b) Gen.t =
    let x1 = ref (g1 ()) in
    let x2 = ref (g2 ()) in
    fun () -> match !x1, !x2 with
      | None, _ | _, None -> None
      | Some x, Some y ->
        (* fetch next elements from g1 and g2 *)
        x1 := g1 ();
        x2 := g2 ();
        Some (x,y)
  ```

- `Iter` is an *internal* iterator. When one wishes to iterate over
  an `'a Iter.t`, one has to give a callback `f : 'a -> unit`
  that is called in succession over every element of the sequence.
  Control is not handed back to the caller before the whole iteration is over.
  This makes `zip` impossible to implement. However, the type `'a Iter.t`
  is general enough that it can be extracted from any classic `iter` function,
  including from data structures such as `Map.S.t` or `Set.S.t` or `Hashtbl.t`;
  one cannot obtain a `'a Gen.t` from these without having access to the internal
  data structure.

In short, `'a Gen.t` is more expressive than `'a Iter.t`, but it also
requires more knowledge of the underlying source of items.
For some operations such as `map` or `flat_map`, Iter is also extremely
efficient and will, if flambda permits, be totally removed at
compile time (e.g. `Iter.(--)` becomes a for loop, and `Iter.filter`
becomes a if test).

For more details, you can read http://gallium.inria.fr/blog/generators-iterators-control-and-continuations/ .

## License

Iter is available under the BSD license.
