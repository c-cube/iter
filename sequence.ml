(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Transient iterators, that abstract on a finite sequence of elements.} *)

(** Sequence abstract iterator type *)
type 'a t = ('a -> unit) -> unit

type 'a sequence = 'a t

type (+'a, +'b) t2 = ('a -> 'b -> unit) -> unit
  (** Sequence of pairs of values of type ['a] and ['b]. *)

(** Build a sequence from a iter function *)
let from_iter f = f

(** Call the function repeatedly until it returns None. This
    sequence is transient, use {!persistent} if needed! *)
let from_fun f =
  fun k ->
    let rec next () =
      match f () with
      | None -> ()
      | Some x -> (k x; next ())
    in next ()

let empty = fun k -> ()

let singleton x = fun k -> k x

(** Infinite sequence of the same element *)
let repeat x = fun k -> while true do k x done

(** [iterate f x] is the infinite sequence (x, f(x), f(f(x)), ...) *)
let iterate f x =
  let rec iterate k x = k x; iterate k (f x) in
  from_iter (fun k -> iterate k x)

(** Sequence that calls the given function to produce elements *)
let forever f =
  let rec forever k = k (f ()); forever k in
  from_iter forever

(** Cycle forever through the given sequence. O(n). *)
let cycle s = fun k -> while true do s k; done

(** Consume the sequence, passing all its arguments to the function *)
let iter f seq = seq f

(** Iterate on elements and their index in the sequence *)
let iteri f seq =
  let r = ref 0 in
  let k x =
    f !r x;
    incr r
  in seq k

(** Fold over elements of the sequence, consuming it *)
let fold f init seq =
  let r = ref init in
  seq (fun elt -> r := f !r elt);
  !r

(** Fold over elements of the sequence and their index, consuming it *)
let foldi f init seq =
  let i = ref 0 in
  let r = ref init in
  seq (fun elt ->
    r := f !r !i elt;
    incr i);
  !r

(** Map objects of the sequence into other elements, lazily *)
let map f seq =
  let seq_fun' k = seq (fun x -> k (f x)) in
  seq_fun'

(** Map objects, along with their index in the sequence *)
let mapi f seq =
  let seq_fun' k =
    let i = ref 0 in
    seq (fun x -> k (f !i x); incr i) in
  seq_fun'

(** Filter on elements of the sequence *)
let filter p seq =
  let seq_fun' k = seq (fun x -> if p x then k x) in
  seq_fun'

(** Append two sequences *)
let append s1 s2 =
  let seq_fun k = s1 k; s2 k in
  seq_fun

(** Concatenate a sequence of sequences into one sequence *)
let concat s =
  from_iter (fun k ->
    (* function that is called on every sub-sequence *)
    let k_seq seq = iter k seq in
    s k_seq)

let flatten s = concat s

(** Monadic bind. It applies the function to every element of the
    initial sequence, and calls [concat]. *)
let flatMap f seq =
  from_iter
    (fun k -> seq (fun x -> (f x) k))

let fmap f seq =
  from_iter
    (fun k ->
      seq (fun x -> match f x with
          | None -> ()
          | Some y -> k y))

(** Insert the given element between every element of the sequence *)
let intersperse elem seq =
  fun k ->
    let first = ref true in
    seq (fun x -> (if !first then first := false else k elem); k x)

(** Mutable unrolled list to serve as intermediate storage *)
module MList = struct
  type 'a node =
    | Nil
    | Cons of 'a array * int ref * 'a node ref

  (* build and call callback on every element *)
  let of_seq_with ?(init_size=8) seq k =
    let start = ref Nil in
    let chunk_size = ref init_size in
    (* fill the list. prev: tail-reference from previous node *)
    let prev, cur = ref start, ref Nil in
    seq
      (fun x ->
        k x;  (* callback *)
        match !cur with
        | Nil ->
          let n = !chunk_size in
          if n < 4096 then chunk_size := 2 * !chunk_size;
          cur := Cons (Array.make n x, ref 1, ref Nil)
        | Cons (a,n,next) ->
          assert (!n < Array.length a);
          a.(!n) <- x;
          incr n;
          if !n = Array.length a then begin
            !prev := !cur;
            prev := next;
            cur := Nil
          end
      );
    !prev := !cur;
    !start

  let of_seq ?init_size seq =
    of_seq_with seq ?init_size (fun _ -> ())

  let is_empty = function
    | Nil -> true
    | Cons _ -> false

  let rec iter f l = match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
        for i=0 to !n - 1 do f a.(i) done;
        iter f !tl

  let iteri f l =
    let rec iteri i f l = match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
        for j=0 to !n - 1 do f (i+j) a.(j) done;
        iteri (i+ !n) f !tl
    in iteri 0 f l

  let rec iter_rev f l = match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
        iter_rev f !tl;
        for i = !n-1 downto 0 do f a.(i) done

  let length l =
    let rec len acc l = match l with
      | Nil -> acc
      | Cons (_, n, tl) -> len (acc+ !n) !tl
    in len 0 l

  (** Get element by index *)
  let rec get l i = match l with
    | Nil -> raise (Invalid_argument "MList.get")
    | Cons (a, n, _) when i < !n -> a.(i)
    | Cons (_, n, tl) -> get !tl (i- !n)

  let to_seq l k = iter k l

  let _to_next arg l =
    let cur = ref l in
    let i = ref 0 in (* offset in cons *)
    let rec get_next _ = match !cur with
      | Nil -> None
      | Cons (_, n, tl) when !i = !n ->
          cur := !tl;
          i := 0;
          get_next arg
      | Cons (a, n, _) ->
          let x = a.(!i) in
          incr i;
          Some x
    in get_next

  let to_gen l = _to_next () l

  let to_stream l =
    Stream.from (_to_next 42 l)  (* 42=magic cookiiiiiie *)
end

(** Iterate on the sequence, storing elements in a data structure.
    The resulting sequence can be iterated on as many times as needed. *)
let persistent ?init_size seq =
  let l = MList.of_seq ?init_size seq in
  MList.to_seq l

type 'a lazy_state =
  | LazySuspend
  | LazyCached of 'a t

let persistent_lazy ?init_size (seq:'a t) =
  let r = ref LazySuspend in
  fun k ->
    match !r with
    | LazyCached seq' -> seq' k
    | LazySuspend ->
        (* here if this traversal is interruted, no caching occurs *)
        let seq' = MList.of_seq_with ?init_size seq k in
        r := LazyCached (MList.to_seq seq')

(** Sort the sequence. Eager, O(n) ram and O(n ln(n)) time. *)
let sort ?(cmp=Pervasives.compare) seq =
  (* use an intermediate list, then sort the list *)
  let l = fold (fun l x -> x::l) [] seq in
  let l = List.fast_sort cmp l in
  fun k -> List.iter k l

(** Group equal consecutive elements. *)
let group ?(eq=fun x y -> x = y) seq =
  fun k ->
    let cur = ref [] in
    seq (fun x ->
      match !cur with
      | [] -> cur := [x]
      | (y::_) as l when eq x y ->
        cur := x::l  (* [x] belongs to the group *)
      | (_::_) as l ->
        k l; (* yield group, and start another one *)
        cur := [x]);
    (* last list *)
    if !cur <> [] then k !cur

(** Remove consecutive duplicate elements. Basically this is
    like [fun seq -> map List.hd (group seq)]. *)
let uniq ?(eq=fun x y -> x = y) seq =
  fun k ->
    let has_prev = ref false
    and prev = ref (Obj.magic 0) in  (* avoid option type, costly *)
    seq (fun x ->
      if !has_prev && eq !prev x
        then ()  (* duplicate *)
        else begin
          has_prev := true;
          prev := x;
          k x
        end)

(** Sort the sequence and remove duplicates. Eager, same as [sort] *)
let sort_uniq ?(cmp=Pervasives.compare) seq =
  let seq' = sort ~cmp seq in
  uniq ~eq:(fun x y -> cmp x y = 0) seq'

(** Cartesian product of the sequences. *)
let product outer inner =
  let inner = persistent inner in
  from_iter
    (fun k ->
      outer (fun x ->
        inner (fun y -> k (x,y))))

(** [join ~join_row a b] combines every element of [a] with every
    element of [b] using [join_row]. If [join_row] returns None, then
    the two elements do not combine. Assume that [b] allows for multiple
    iterations. *)
let join ~join_row s1 s2 =
  fun k ->
    s1 (fun a ->
      s2 (fun b ->
        match join_row a b with
        | None -> ()
        | Some c -> k c))  (* yield the combination of [a] and [b] *)

(** [unfoldr f b] will apply [f] to [b]. If it
    yields [Some (x,b')] then [x] is returned
    and unfoldr recurses with [b']. *)
let unfoldr f b =
  let rec unfold k b = match f b with
    | None -> ()
    | Some (x, b') -> k x; unfold k b'
  in
  from_iter (fun k -> unfold k b)

(** Sequence of intermediate results *)
let scan f acc seq =
  from_iter
    (fun k ->
      k acc;
      let acc = ref acc in
      seq (fun elt -> let acc' = f !acc elt in k acc'; acc := acc'))

let max ?(lt=fun x y -> x < y) seq =
  let ret = ref None in
  seq (fun x -> match !ret with
    | None -> ret := Some x
    | Some y -> if lt y x then ret := Some x);
  !ret

let min ?(lt=fun x y -> x < y) seq =
  let ret = ref None in
  seq (fun x -> match !ret with
    | None -> ret := Some x
    | Some y -> if lt x y then ret := Some x);
  !ret

exception ExitSequence

(** Take at most [n] elements from the sequence *)
let take n seq =
  let count = ref 0 in
  if n = 0 then empty
    else fun k ->
      try
        seq (fun x ->
          incr count;
          k x;
          if !count = n then raise ExitSequence)
      with ExitSequence -> ()

(** Drop the [n] first elements of the sequence *)
let drop n seq =
  let count = ref 0 in
  fun k -> seq
    (fun x -> if !count >= n then k x else incr count)

(** Reverse the sequence. O(n) memory. *)
let rev seq =
  let l = MList.of_seq seq in
  from_iter (fun k -> MList.iter_rev k l)

(** Do all elements satisfy the predicate? *)
let for_all p seq =
  try
    seq (fun x -> if not (p x) then raise ExitSequence);
    true
  with ExitSequence -> false

(** Exists there some element satisfying the predicate? *)
let exists p seq =
  try
    seq (fun x -> if p x then raise ExitSequence);
    false
  with ExitSequence -> true

(** How long is the sequence? *)
let length seq =
  let r = ref 0 in
  seq (fun _ -> incr r);
  !r

(** Is the sequence empty? *)
let is_empty seq =
  try seq (fun _ -> raise ExitSequence); true
  with ExitSequence -> false

(** {2 Transform a sequence} *)

let empty2 =
  fun k -> ()

let is_empty2 seq2 =
  try ignore (seq2 (fun _ _ -> raise ExitSequence)); true
  with ExitSequence -> false

let length2 seq2 =
  let r = ref 0 in
  seq2 (fun _ _ -> incr r);
  !r

let zip seq2 =
  fun k -> seq2 (fun x y -> k (x,y))

let unzip seq =
  fun k -> seq (fun (x,y) -> k x y)

(** Zip elements of the sequence with their index in the sequence *)
let zip_i seq =
  fun k ->
    let r = ref 0 in
    seq (fun x -> let n = !r in incr r; k n x)

let fold2 f acc seq2 =
  let acc = ref acc in
  seq2 (fun x y -> acc := f !acc x y);
  !acc

let iter2 f seq2 =
  seq2 f

let map2 f seq2 =
  fun k -> seq2 (fun x y -> k (f x y))

(** [map2_2 f g seq2] maps each [x, y] of seq2 into [f x y, g x y] *)
let map2_2 f g seq2 =
  fun k -> seq2 (fun x y -> k (f x y) (g x y))

(** {2 Basic data structures converters} *)

let to_list seq = List.rev (fold (fun y x -> x::y) [] seq)

let to_rev_list seq = fold (fun y x -> x :: y) [] seq
  (** Get the list of the reversed sequence (more efficient) *)

let of_list l = from_iter (fun k -> List.iter k l)

let to_array seq =
  let l = MList.of_seq seq in
  let n = MList.length l in
  if n = 0
    then [||]
    else begin
      let a = Array.make n (MList.get l 0) in
      MList.iteri (fun i x -> a.(i) <- x) l;
      a
    end

let of_array a =
  fun k ->
    for i = 0 to Array.length a - 1 do
      k (Array.unsafe_get a i)
    done

let of_array_i a =
  fun k ->
    for i = 0 to Array.length a - 1 do
      k (i, Array.unsafe_get a i)
    done

let of_array2 a =
  fun k ->
    for i = 0 to Array.length a - 1 do
      k i (Array.unsafe_get a i)
    done

(** [array_slice a i j] Sequence of elements whose indexes range
    from [i] to [j] *)
let array_slice a i j =
  assert (i >= 0 && j < Array.length a);
  fun k ->
    for idx = i to j do
      k a.(idx);  (* iterate on sub-array *)
    done

(** Sequence of elements of a stream (usable only once) *)
let of_stream s =
  let seq k = Stream.iter k s in
  from_iter seq

(** Convert to a stream. The sequence is made persistent. *)
let to_stream seq =
  let l = MList.of_seq seq in
  MList.to_stream l

(** Push elements of the sequence on the stack *)
let to_stack s seq = iter (fun x -> Stack.push x s) seq

(** Sequence of elements of the stack (same order as [Stack.iter]) *)
let of_stack s = from_iter (fun k -> Stack.iter k s)

(** Push elements of the sequence into the queue *)
let to_queue q seq = iter (fun x -> Queue.push x q) seq

(** Sequence of elements contained in the queue, FIFO order *)
let of_queue q = from_iter (fun k -> Queue.iter k q)

let hashtbl_add h seq =
  iter (fun (k,v) -> Hashtbl.add h k v) seq

let hashtbl_replace h seq =
  iter (fun (k,v) -> Hashtbl.replace h k v) seq

let to_hashtbl seq =
  let h = Hashtbl.create 3 in
  hashtbl_replace h seq;
  h

let to_hashtbl2 seq2 =
  let h = Hashtbl.create 3 in
  seq2 (fun k v -> Hashtbl.replace h k v);
  h

let of_hashtbl h =
  from_iter (fun k -> Hashtbl.iter (fun a b -> k (a, b)) h)

let of_hashtbl2 h =
  fun k -> Hashtbl.iter k h

let hashtbl_keys h =
  from_iter (fun k -> Hashtbl.iter (fun a b -> k a) h)

let hashtbl_values h =
  from_iter (fun k -> Hashtbl.iter (fun a b -> k b) h)

let of_str s = from_iter (fun k -> String.iter k s)

let to_str seq =
  let b = Buffer.create 64 in
  iter (fun c -> Buffer.add_char b c) seq;
  Buffer.contents b

let of_in_channel ic =
  from_iter (fun k ->
    try while true do
      let c = input_char ic in k c
    done with End_of_file -> ())

(** Copy content of the sequence into the buffer *)
let to_buffer seq buf =
  iter (fun c -> Buffer.add_char buf c) seq

(** Iterator on integers in [start...stop] by steps 1 *)
let int_range ~start ~stop =
  fun k ->
    for i = start to stop do k i done

let int_range_dec ~start ~stop =
  fun k ->
    for i = start downto stop do k i done

(** Convert the given set to a sequence. The set module must be provided. *)
let of_set (type s) (type v) m set =
  let module S = (val m : Set.S with type t = s and type elt = v) in
  from_iter
    (fun k -> S.iter k set)

(** Convert the sequence to a set, given the proper set module *)
let to_set (type s) (type v) m seq =
  let module S = (val m : Set.S with type t = s and type elt = v) in
  fold
    (fun set x -> S.add x set)
    S.empty seq

type 'a gen = unit -> 'a option

let of_gen g =
  (* consume the generator to build a MList *)
  let rec iter1 k = match g () with
    | None -> ()
    | Some x -> k x; iter1 k
  in
  let l = MList.of_seq iter1 in
  MList.to_seq l

let to_gen seq =
  let l = MList.of_seq seq in
  MList.to_gen l

(** {2 Functorial conversions between sets and sequences} *)

module Set = struct
  module type S = sig
    include Set.S
    val of_seq : elt sequence -> t
    val to_seq : t -> elt sequence
  end

  (** Create an enriched Set module from the given one *)
  module Adapt(X : Set.S) = struct
    let to_seq set = from_iter (fun k -> X.iter k set)

    let of_seq seq = fold (fun set x -> X.add x set) X.empty seq

    include X
  end

  (** Functor to build an extended Set module from an ordered type *)
  module Make(X : Set.OrderedType) = struct
    module MySet = Set.Make(X)
    include Adapt(MySet)
  end
end

(** {2 Conversion between maps and sequences.} *)

module Map = struct
  module type S = sig
    include Map.S
    val to_seq : 'a t -> (key * 'a) sequence
    val of_seq : (key * 'a) sequence -> 'a t
    val keys : 'a t -> key sequence
    val values : 'a t -> 'a sequence
  end

  (** Adapt a pre-existing Map module to make it sequence-aware *)
  module Adapt(M : Map.S) = struct
    let to_seq m = from_iter (fun k -> M.iter (fun x y -> k (x,y)) m)

    let of_seq seq = fold (fun m (k,v) -> M.add k v m) M.empty seq

    let keys m = from_iter (fun k -> M.iter (fun x _ -> k x) m)

    let values m = from_iter (fun k -> M.iter (fun _ y -> k y) m)

    include M
  end

  (** Create an enriched Map module, with sequence-aware functions *)
  module Make(V : Map.OrderedType) : S with type key = V.t = struct
    module M = Map.Make(V)
    include Adapt(M)
  end
end

(** {2 Infinite sequences of random values} *)

let random_int bound = forever (fun () -> Random.int bound)

let random_bool = forever Random.bool

let random_float bound = forever (fun () -> Random.float bound)

(** Sequence of choices of an element in the array *)
let random_array a =
  assert (Array.length a > 0);
  let seq k =
    while true do
      let i = Random.int (Array.length a) in
      k a.(i);
    done in
  from_iter seq

let random_list l = random_array (Array.of_list l)

(** {2 Infix functions} *)

module Infix = struct
  let (--) i j = int_range ~start:i ~stop:j

  let (--^) i j = int_range_dec ~start:i ~stop:j
end

include Infix

(** {2 Pretty printing of sequences} *)

(** Pretty print a sequence of ['a], using the given pretty printer
    to print each elements. An optional separator string can be provided. *)
let pp_seq ?(sep=", ") pp_elt formatter seq =
  let first = ref true in
  iter
    (fun x -> 
      (if !first then first := false
        else begin
          Format.pp_print_string formatter sep;
          Format.pp_print_cut formatter ();
        end);
      pp_elt formatter x)
    seq

let pp_buf ?(sep=", ") pp_elt buf seq =
  let first = ref true in
  iter
    (fun x -> 
      if !first then first := false else Buffer.add_string buf sep;
      pp_elt buf x)
    seq

let to_string ?sep pp_elt seq =
  let buf = Buffer.create 25 in
  pp_buf ?sep (fun buf x -> Buffer.add_string buf (pp_elt x)) buf seq;
  Buffer.contents buf
