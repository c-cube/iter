
(* This file is free software, part of sequence. See file "license" for more details. *)

(** {1 Simple and Efficient Iterators} *)

(** The iterators are designed to allow easy transfer (mappings) between data
    structures, without defining [n^2] conversions between the [n] types. The
    implementation relies on the assumption that a sequence can be iterated
    on as many times as needed; this choice allows for high performance
    of many combinators. However, for transient iterators, the {!persistent}
    function is provided, storing elements of a transient iterator
    in memory; the iterator can then be used several times (See further).

    Note that some combinators also return sequences (e.g. {!group}). The
    transformation is computed on the fly every time one iterates over
    the resulting sequence. If a transformation performs heavy computation,
    {!persistent} can also be used as intermediate storage.

    Most functions are {b lazy}, i.e. they do not actually use their arguments
    until their result is iterated on. For instance, if one calls {!map}
    on a sequence, one gets a new sequence, but nothing else happens until
    this new sequence is used (by folding or iterating on it).

    If a sequence is built from an iteration function that is {b repeatable}
    (i.e. calling it several times always iterates on the same set of
    elements, for instance List.iter or Map.iter), then
    the resulting {!t} object is also repeatable. For {b one-time iter functions}
    such as iteration on a file descriptor or a {!Stream},
    the {!persistent} function can be used to iterate and store elements in
    a memory structure; the result is a sequence that iterates on the elements
    of this memory structure, cheaply and repeatably. *)

type +'a t = ('a -> unit) -> unit
(** A sequence of values of type ['a]. If you give it a function ['a -> unit]
    it will be applied to every element of the sequence successively. *)

type +'a sequence = 'a t

type (+'a, +'b) t2 = ('a -> 'b -> unit) -> unit
(** Sequence of pairs of values of type ['a] and ['b]. *)

type 'a equal = 'a -> 'a -> bool
type 'a hash = 'a -> int

(** {2 Build a sequence} *)

val from_iter : (('a -> unit) -> unit) -> 'a t
(** Build a sequence from a iter function *)

val from_fun : (unit -> 'a option) -> 'a t
(** Call the function repeatedly until it returns None. This
    sequence is transient, use {!persistent} if needed! *)

val empty : 'a t
(** Empty sequence. It contains no element. *)

val singleton : 'a -> 'a t
(** Singleton sequence, with exactly one element. *)

val doubleton : 'a -> 'a -> 'a t
(** Sequence with exactly two elements *)

val init : (int -> 'a) -> 'a t
(** [init f] is the infinite sequence [f 0; f 1; f 2; …].
    @since 0.9 *)

val cons : 'a -> 'a t -> 'a t
(** [cons x l] yields [x], then yields from [l].
    Same as [append (singleton x) l] *)

val snoc : 'a t -> 'a -> 'a t
(** Same as {!cons} but yields the element after iterating on [l] *)

val return : 'a -> 'a t
(** Synonym to {!singleton} *)

val pure : 'a -> 'a t
(** Synonym to {!singleton} *)

val repeat : 'a -> 'a t
(** Infinite sequence of the same element. You may want to look
    at {!take} and the likes if you iterate on it. *)

val iterate : ('a -> 'a) -> 'a -> 'a t
(** [iterate f x] is the infinite sequence [x, f(x), f(f(x)), ...] *)

val forever : (unit -> 'b) -> 'b t
(** Sequence that calls the given function to produce elements.
    The sequence may be transient (depending on the function), and definitely
    is infinite. You may want to use {!take} and {!persistent}. *)

val cycle : 'a t -> 'a t
(** Cycle forever through the given sequence. Assume the given sequence can
    be traversed any amount of times (not transient).  This yields an
    infinite sequence, you should use something like {!take} not to loop
    forever. *)

(** {2 Consume a sequence} *)

val iter : ('a -> unit) -> 'a t -> unit
(** Consume the sequence, passing all its arguments to the function.
    Basically [iter f seq] is just [seq f]. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Iterate on elements and their index in the sequence *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold over elements of the sequence, consuming it *)

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Fold over elements of the sequence and their index, consuming it *)

val fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'b t
(** [fold_map f acc l] is like {!map}, but it carries some state as in
    {!fold}. The state is not returned, it is just used to thread some
    information to the map function.
    @since 0.9 *)

val fold_filter_map : ('acc -> 'a -> 'acc * 'b option) -> 'acc -> 'a t -> 'b t
(** [fold_filter_map f acc l] is a {!fold_map}-like function, but the
    function can choose to skip an element by retuning [None].
    @since 0.9 *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map objects of the sequence into other elements, lazily *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Map objects, along with their index in the sequence *)

val map_by_2 : ('a -> 'a -> 'a) -> 'a t -> 'a t
  (** Map objects two by two. lazily.
      The last element is kept in the sequence if the count is odd.
      @since 0.7 *)

val for_all : ('a -> bool) -> 'a t -> bool
(** Do all elements satisfy the predicate? *)

val exists : ('a -> bool) -> 'a t -> bool
(** Exists there some element satisfying the predicate? *)

val mem : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** Is the value a member of the sequence?
    @param eq the equality predicate to use (default [(=)])
    @since 0.5 *)

val find : ('a -> 'b option) -> 'a t -> 'b option
(** Find the first element on which the function doesn't return [None]
    @since 0.5 *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** Alias to {!find}
    @since 0.10 *)

val findi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Indexed version of {!find}
    @since 0.9 *)

val find_mapi : (int -> 'a -> 'b option) -> 'a t -> 'b option
(** Alias to {!findi}
    @since 0.10 *)

val find_pred : ('a -> bool) -> 'a t -> 'a option
(** [find_pred p l] finds the first element of [l] that satisfies [p],
    or returns [None] if no element satisfies [p]
    @since 0.9 *)

val find_pred_exn : ('a -> bool) -> 'a t -> 'a
(** Unsafe version of {!find_pred}
    @raise Not_found if no such element is found
    @since 0.9 *)

val length : 'a t -> int
(** How long is the sequence? Forces the sequence. *)

val is_empty : 'a t -> bool
(** Is the sequence empty? Forces the sequence. *)

(** {2 Transform a sequence} *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter on elements of the sequence *)

val append : 'a t -> 'a t -> 'a t
(** Append two sequences. Iterating on the result is like iterating
    on the first, then on the second. *)

val concat : 'a t t -> 'a t
(** Concatenate a sequence of sequences into one sequence. *)

val flatten : 'a t t -> 'a t
(** Alias for {!concat} *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Monadic bind. Intuitively, it applies the function to every
    element of the initial sequence, and calls {!concat}.
    Formerly [flatMap]
    @since 0.5 *)

val flat_map_l : ('a -> 'b list) -> 'a t -> 'b t
(** Convenience function combining {!flat_map} and {!of_list}
    @since 0.9 *)

val seq_list : 'a t list -> 'a list t
(** [seq_list l] returns all the ways to pick one element in each sub-sequence
    in [l]. Assumes the sub-sequences can be iterated on several times.
    @since NEXT_RELEASE *)

val seq_list_map : ('a -> 'b t) -> 'a list -> 'b list t
(** [seq_list_map f l] maps [f] over every element of [l],
    then calls {!seq_list}
    @since NEXT_RELEASE *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Map and only keep non-[None] elements
    Formerly [fmap]
    @since 0.5 *)

val intersperse : 'a -> 'a t -> 'a t
(** Insert the single element between every element of the sequence *)

(** {2 Caching} *)

val persistent : 'a t -> 'a t
(** Iterate on the sequence, storing elements in an efficient internal structure..
    The resulting sequence can be iterated on as many times as needed.
    {b Note}: calling persistent on an already persistent sequence
    will still make a new copy of the sequence! *)

val persistent_lazy : 'a t -> 'a t
(** Lazy version of {!persistent}. When calling [persistent_lazy s],
    a new sequence [s'] is immediately returned (without actually consuming
    [s]) in constant time; the first time [s'] is iterated on,
    it also consumes [s] and caches its content into a inner data
    structure that will back [s'] for future iterations.

    {b warning}: on the first traversal of [s'], if the traversal
    is interrupted prematurely ({!take}, etc.) then [s'] will not be
    memorized, and the next call to [s'] will traverse [s] again. *)

(** {2 Misc} *)

val sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** Sort the sequence. Eager, O(n) ram and O(n ln(n)) time.
    It iterates on elements of the argument sequence immediately,
    before it sorts them. *)

val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** Sort the sequence and remove duplicates. Eager, same as [sort] *)

val sorted : ?cmp:('a -> 'a -> int) -> 'a t -> bool
(** Checks whether the sequence is sorted. Eager, same as {!sort}.
    @since 0.9 *)

val group_succ_by : ?eq:('a -> 'a -> bool) -> 'a t -> 'a list t
(** Group equal consecutive elements.
    Formerly synonym to [group].
    @since 0.6 *)

val group_by : ?hash:('a -> int) -> ?eq:('a -> 'a -> bool) ->
  'a t -> 'a list t
(** Group equal elements, disregarding their order of appearance.
    The result sequence is traversable as many times as required.
    @since 0.6 *)

val count : ?hash:('a -> int) -> ?eq:('a -> 'a -> bool) ->
  'a t -> ('a * int) t
(** Map each distinct element to its number of occurrences in the whole seq.
    Similar to [group_by seq |> map (fun l->List.hd l, List.length l)]
    @since 0.10 *)

val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
(** Remove consecutive duplicate elements. Basically this is
    like [fun seq -> map List.hd (group seq)]. *)

val product : 'a t -> 'b t -> ('a * 'b) t
(** Cartesian product of the sequences. When calling [product a b],
    the caller {b MUST} ensure that [b] can be traversed as many times
    as required (several times), possibly by calling {!persistent} on it
    beforehand. *)

val diagonal_l : 'a list -> ('a * 'a) t
(** All pairs of distinct positions of the list. [diagonal l] will
    return the sequence of all [List.nth i l, List.nth j l] if [i < j].
    @since 0.9 *)

val diagonal : 'a t -> ('a * 'a) t
(** All pairs of distinct positions of the sequence.
    Iterates only once on the sequence, which must be finite.
    @since 0.9 *)

val product2 : 'a t -> 'b t -> ('a, 'b) t2
(** Binary version of {!product}. Same requirements.
    @since 0.5 *)

val join : join_row:('a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
(** [join ~join_row a b] combines every element of [a] with every
    element of [b] using [join_row]. If [join_row] returns None, then
    the two elements do not combine. Assume that [b] allows for multiple
    iterations. *)

val join_by : ?eq:'key equal -> ?hash:'key hash ->
  ('a -> 'key) -> ('b -> 'key) ->
  merge:('key -> 'a -> 'b -> 'c option) ->
  'a t ->
  'b t ->
  'c t
(** [join key1 key2 ~merge] is a binary operation
    that takes two sequences [a] and [b], projects their
    elements resp. with [key1] and [key2], and combine
    values [(x,y)] from [(a,b)] with the same [key]
    using [merge]. If [merge] returns [None], the combination
    of values is discarded.
    @since 0.10 *)

val join_all_by : ?eq:'key equal -> ?hash:'key hash ->
  ('a -> 'key) -> ('b -> 'key) ->
  merge:('key -> 'a list -> 'b list -> 'c option) ->
  'a t ->
  'b t ->
  'c t
(** [join_all_by key1 key2 ~merge] is a binary operation
    that takes two sequences [a] and [b], projects their
    elements resp. with [key1] and [key2], and, for each key [k]
    occurring in at least one of them:
    - compute the list [l1] of elements of [a] that map to [k]
    - compute the list [l2] of elements of [b] that map to [k]
    - call [merge k l1 l2]. If [merge] returns [None], the combination
      of values is discarded, otherwise it returns [Some c]
      and [c] is inserted in the result.
    @since 0.10 *)

val group_join_by : ?eq:'a equal -> ?hash:'a hash ->
  ('b -> 'a) ->
  'a t ->
  'b t ->
  ('a * 'b list) t
(** [group_join_by key2] associates to every element [x] of
    the first sequence, all the elements [y] of the second
    sequence such that [eq x (key y)]. Elements of the first
    sequences without corresponding values in the second one
    are mapped to [[]]
    @since 0.10 *)

val inter :
  ?eq:'a equal -> ?hash:'a hash ->
  'a t -> 'a t -> 'a t
(** Intersection of two collections. Each element will occur at most once
    in the result. Eager.
    @since 0.10 *)

(*$=
  [2;4;5;6] (inter (1--6) (cons 2 (4--10)) |> sort |> to_list)
  [] (inter (0--5) (6--10) |> to_list)
*)

val union :
  ?eq:'a equal -> ?hash:'a hash ->
  'a t -> 'a t -> 'a t
(** Union of two collections. Each element will occur at most once
    in the result. Eager.
    @since 0.10 *)

(*$=
  [2;4;5;6] (union (4--6) (cons 2 (4--5)) |> sort |> to_list)
*)

val diff :
  ?eq:'a equal -> ?hash:'a hash ->
  'a t -> 'a t -> 'a t
(** Set difference. Eager.
    @since 0.10 *)

(*$=
  [1;2;8;9;10] (diff (1--10) (3--7) |> to_list)
*)

val subset :
  ?eq:'a equal -> ?hash:'a hash ->
  'a t -> 'a t -> bool
(** [subset a b] returns [true] if all elements of [a] belong to [b]. Eager.
    @since 0.10 *)

(*$T
  subset (2 -- 4) (1 -- 4)
  not (subset (1 -- 4) (2 -- 10))
*)

val unfoldr : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** [unfoldr f b] will apply [f] to [b]. If it
    yields [Some (x,b')] then [x] is returned
    and unfoldr recurses with [b']. *)

val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
(** Sequence of intermediate results *)

val max : ?lt:('a -> 'a -> bool) -> 'a t -> 'a option
(** Max element of the sequence, using the given comparison function.
    @return None if the sequence is empty, Some [m] where [m] is the maximal
    element otherwise *)

val max_exn : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
(** Unsafe version of {!max}
    @raise Not_found if the sequence is empty
    @since 0.10 *)

val min : ?lt:('a -> 'a -> bool) -> 'a t -> 'a option
(** Min element of the sequence, using the given comparison function.
    see {!max} for more details. *)

val min_exn : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
(** Unsafe version of {!min}
    @raise Not_found if the sequence is empty
    @since 0.10 *)

val head : 'a t -> 'a option
(** First element, if any, otherwise [None]
    @since 0.5.1 *)

val head_exn : 'a t -> 'a
(** First element, if any, fails
    @raise Invalid_argument if the sequence is empty
    @since 0.5.1 *)

val take : int -> 'a t -> 'a t
(** Take at most [n] elements from the sequence. Works on infinite
    sequences. *)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** Take elements while they satisfy the predicate, then stops iterating.
    Will work on an infinite sequence [s] if the predicate is false for at
    least one element of [s]. *)

val fold_while : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
(** Folds over elements of the sequence, stopping early if the accumulator
    returns [('a, `Stop)]
    @since 0.5.5 *)

val drop : int -> 'a t -> 'a t
(** Drop the [n] first elements of the sequence. Lazy. *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** Predicate version of {!drop} *)

val rev : 'a t -> 'a t
(** Reverse the sequence. O(n) memory and time, needs the
    sequence to be finite. The result is persistent and does
    not depend on the input being repeatable. *)

(** {2 Binary sequences} *)

val empty2 : ('a, 'b) t2

val is_empty2 : (_, _) t2 -> bool

val length2 : (_, _) t2 -> int

val zip : ('a, 'b) t2 -> ('a * 'b) t

val unzip : ('a * 'b) t -> ('a, 'b) t2

val zip_i : 'a t -> (int, 'a) t2
(** Zip elements of the sequence with their index in the sequence *)

val fold2 : ('c -> 'a -> 'b -> 'c) -> 'c -> ('a, 'b) t2 -> 'c

val iter2 : ('a -> 'b -> unit) -> ('a, 'b) t2 -> unit

val map2 : ('a -> 'b -> 'c) -> ('a, 'b) t2 -> 'c t

val map2_2 : ('a -> 'b -> 'c) -> ('a -> 'b -> 'd) -> ('a, 'b) t2 -> ('c, 'd) t2
(** [map2_2 f g seq2] maps each [x, y] of seq2 into [f x y, g x y] *)

(** {2 Basic data structures converters} *)

val to_list : 'a t -> 'a list
(** Convert the sequence into a list. Preserves order of elements.
    This function is tail-recursive, but consumes 2*n memory.
    If order doesn't matter to you, consider {!to_rev_list}. *)

val to_rev_list : 'a t -> 'a list
(** Get the list of the reversed sequence (more efficient than {!to_list}) *)

val of_list : 'a list -> 'a t

val on_list : ('a t -> 'b t) -> 'a list -> 'b list
(** [on_list f l] is equivalent to [to_list @@ f @@ of_list l].
    @since 0.5.2
*)

val to_opt : 'a t -> 'a option
(** Alias to {!head}
    @since 0.5.1 *)

val to_array : 'a t -> 'a array
(** Convert to an array. Currently not very efficient because
    an intermediate list is used. *)

val of_array : 'a array -> 'a t

val of_array_i : 'a array -> (int * 'a) t
(** Elements of the array, with their index *)

val of_array2 : 'a array -> (int, 'a) t2

val array_slice : 'a array -> int -> int -> 'a t
(** [array_slice a i j] Sequence of elements whose indexes range
    from [i] to [j] *)

val of_opt : 'a option -> 'a t
(** Iterate on 0 or 1 values.
    @since 0.5.1 *)

val of_stream : 'a Stream.t -> 'a t
(** Sequence of elements of a stream (usable only once) *)

val to_stream : 'a t -> 'a Stream.t
(** Convert to a stream. linear in memory and time (a copy is made in memory) *)

val to_stack : 'a Stack.t -> 'a t -> unit
(** Push elements of the sequence on the stack *)

val of_stack : 'a Stack.t -> 'a t
(** Sequence of elements of the stack (same order as [Stack.iter]) *)

val to_queue : 'a Queue.t -> 'a t -> unit
(** Push elements of the sequence into the queue *)

val of_queue : 'a Queue.t -> 'a t
(** Sequence of elements contained in the queue, FIFO order *)

val hashtbl_add : ('a, 'b) Hashtbl.t -> ('a * 'b) t -> unit
(** Add elements of the sequence to the hashtable, with
    Hashtbl.add *)

val hashtbl_replace : ('a, 'b) Hashtbl.t -> ('a * 'b) t -> unit
(** Add elements of the sequence to the hashtable, with
    Hashtbl.replace (erases conflicting bindings) *)

val to_hashtbl : ('a * 'b) t -> ('a, 'b) Hashtbl.t
(** Build a hashtable from a sequence of key/value pairs *)

val to_hashtbl2 : ('a, 'b) t2 -> ('a, 'b) Hashtbl.t
(** Build a hashtable from a sequence of key/value pairs *)

val of_hashtbl : ('a, 'b) Hashtbl.t -> ('a * 'b) t
(** Sequence of key/value pairs from the hashtable *)

val of_hashtbl2 : ('a, 'b) Hashtbl.t -> ('a, 'b) t2
(** Sequence of key/value pairs from the hashtable *)

val hashtbl_keys : ('a, 'b) Hashtbl.t -> 'a t
val hashtbl_values : ('a, 'b) Hashtbl.t -> 'b t

val of_str : string -> char t
val to_str :  char t -> string

val concat_str : string t -> string
(** Concatenate strings together, eagerly.
    Also see {!intersperse} to add a separator.
    @since 0.5 *)

exception OneShotSequence
(** Raised when the user tries to iterate several times on
    a transient iterator *)

val of_in_channel : in_channel -> char t
(** Iterates on characters of the input (can block when one
    iterates over the sequence). If you need to iterate
    several times on this sequence, use {!persistent}.
    @raise OneShotSequence when used more than once. *)

val to_buffer : char t -> Buffer.t -> unit
(** Copy content of the sequence into the buffer *)

val int_range : start:int -> stop:int -> int t
(** Iterator on integers in [start...stop] by steps 1. Also see
    {!(--)} for an infix version. *)

val int_range_dec : start:int -> stop:int -> int t
(** Iterator on decreasing integers in [stop...start] by steps -1.
    See {!(--^)} for an infix version *)

val int_range_by : step:int -> int -> int -> int t
(** [int_range_by ~step i j] is the range starting at [i], including [j],
    where the difference between successive elements is [step].
    use a negative [step] for a decreasing sequence.
    @raise Invalid_argument if [step=0] *)

val bools : bool t
(** Iterates on [true] and [false]
    @since 0.7 *)

val of_set : (module Set.S with type elt = 'a and type t = 'b) -> 'b -> 'a t
(** Convert the given set to a sequence. The set module must be provided. *)

val to_set : (module Set.S with type elt = 'a and type t = 'b) -> 'a t -> 'b
(** Convert the sequence to a set, given the proper set module *)

type 'a gen = unit -> 'a option
type 'a klist = unit -> [`Nil | `Cons of 'a * 'a klist]

val of_gen : 'a gen -> 'a t
(** Traverse eagerly the generator and build a sequence from it *)

val to_gen : 'a t -> 'a gen
(** Make the sequence persistent (O(n)) and then iterate on it. Eager. *)

val of_klist : 'a klist -> 'a t
(** Iterate on the lazy list *)

val to_klist : 'a t -> 'a klist
(** Make the sequence persistent and then iterate on it. Eager. *)

(** {2 Functorial conversions between sets and sequences} *)

module Set : sig
  module type S = sig
    include Set.S
    val of_seq : elt sequence -> t
    val to_seq : t -> elt sequence
    val to_list : t -> elt list
    val of_list : elt list -> t
  end

  (** Create an enriched Set module from the given one *)
  module Adapt(X : Set.S) : S with type elt = X.elt and type t = X.t

  (** Functor to build an extended Set module from an ordered type *)
  module Make(X : Set.OrderedType) : S with type elt = X.t
end

(** {2 Conversion between maps and sequences.} *)

module Map : sig
  module type S = sig
    include Map.S
    val to_seq : 'a t -> (key * 'a) sequence
    val of_seq : (key * 'a) sequence -> 'a t
    val keys : 'a t -> key sequence
    val values : 'a t -> 'a sequence
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
  end

  (** Adapt a pre-existing Map module to make it sequence-aware *)
  module Adapt(M : Map.S) : S with type key = M.key and type 'a t = 'a M.t

  (** Create an enriched Map module, with sequence-aware functions *)
  module Make(V : Map.OrderedType) : S with type key = V.t
end

(** {2 Infinite sequences of random values} *)

val random_int : int -> int t
(** Infinite sequence of random integers between 0 and
    the given higher bound (see Random.int) *)

val random_bool : bool t
(** Infinite sequence of random bool values *)

val random_float : float -> float t

val random_array : 'a array -> 'a t
(** Sequence of choices of an element in the array *)

val random_list : 'a list -> 'a t
(** Infinite sequence of random elements of the list. Basically the
    same as {!random_array}. *)

val shuffle : 'a t -> 'a t
(** [shuffle seq] returns a perfect shuffle of [seq].
    Uses O(length seq) memory and time. Eager.
    @since 0.7 *)

val shuffle_buffer : int -> 'a t -> 'a t
(** [shuffle_buffer n seq] returns a sequence of element of [seq] in random
    order. The shuffling is *not* uniform. Uses O(n) memory.

    The first [n] elements of the sequence are consumed immediately. The
    rest is consumed lazily.
    @since 0.7 *)

(** {2 Sampling} *)

val sample : int -> 'a t -> 'a array
  (** [sample n seq] returns k samples of [seq], with uniform probability.
      It will consume the sequence and use O(n) memory.

      It returns an array of size [min (length seq) n].
      @since 0.7 *)

(** {2 Infix functions} *)

module Infix : sig
  val (--) : int -> int -> int t
  (** [a -- b] is the range of integers from [a] to [b], both included,
      in increasing order. It will therefore be empty if [a > b]. *)

  val (--^) : int -> int -> int t
  (** [a --^ b] is the range of integers from [b] to [a], both included,
      in decreasing order (starts from [a]).
      It will therefore be empty if [a < b]. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind (infix version of {!flat_map}
      @since 0.5 *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix version of {!map}
      @since 0.5 *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** Applicative operator (product+application)
      @since 0.5 *)

  val (<+>) : 'a t -> 'a t -> 'a t
  (** Concatenation of sequences
      @since 0.5 *)
end

include module type of Infix

(** {2 Pretty printing of sequences} *)

val pp_seq : ?sep:string -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit
(** Pretty print a sequence of ['a], using the given pretty printer
    to print each elements. An optional separator string can be provided. *)

val pp_buf : ?sep:string -> (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a t -> unit
(** Print into a buffer *)

val to_string : ?sep:string -> ('a -> string) -> 'a t -> string
(** Print into a string *)

(** {2 Basic IO}

    Very basic interface to manipulate files as sequence of chunks/lines. The
    sequences take care of opening and closing files properly; every time
    one iterates over a sequence, the file is opened/closed again.

    Example: copy a file ["a"] into file ["b"], removing blank lines:

    {[
      Sequence.(IO.lines_of "a" |> filter (fun l-> l<> "") |> IO.write_lines "b");;
    ]}

    By chunks of [4096] bytes:

    {[
      Sequence.IO.(chunks_of ~size:4096 "a" |> write_to "b");;
    ]}

    Read the lines of a file into a list:

    {[
      Sequence.IO.lines "a" |> Sequence.to_list
    ]}

    @since 0.5.1 *)

module IO : sig
  val lines_of : ?mode:int -> ?flags:open_flag list ->
    string -> string t
  (** [lines_of filename] reads all lines of the given file. It raises the
      same exception as would opening the file and read from it, except
      from [End_of_file] (which is caught). The file is {b always} properly
      closed.
      Every time the sequence is iterated on, the file is opened again, so
      different iterations might return different results
      @param mode default [0o644]
      @param flags default: [[Open_rdonly]] *)

  val chunks_of : ?mode:int -> ?flags:open_flag list -> ?size:int ->
    string -> string t
  (** Read chunks of the given [size] from the file. The last chunk might be
      smaller. Behaves like {!lines_of} regarding errors and options.
      Every time the sequence is iterated on, the file is opened again, so
      different iterations might return different results *)

  val write_to : ?mode:int -> ?flags:open_flag list ->
    string -> string t -> unit
  (** [write_to filename seq] writes all strings from [seq] into the given
      file. It takes care of opening and closing the file.
      @param mode default [0o644]
      @param flags used by [open_out_gen]. Default: [[Open_creat;Open_wronly]]. *)

  val write_bytes_to : ?mode:int -> ?flags:open_flag list ->
    string -> Bytes.t t -> unit
  (** @since 0.5.4 *)

  val write_lines : ?mode:int -> ?flags:open_flag list ->
    string -> string t -> unit
  (** Same as {!write_to}, but intercales ['\n'] between each string *)

  val write_bytes_lines : ?mode:int -> ?flags:open_flag list ->
    string -> Bytes.t t -> unit
  (** @since 0.5.4 *)
end
