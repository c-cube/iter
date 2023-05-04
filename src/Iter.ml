(* This file is free software, part of iter. See file "license" for more details. *)

type 'a t = ('a -> unit) -> unit
(** Iter abstract iterator type *)

type 'a iter = 'a t
type 'a equal = 'a -> 'a -> bool
type 'a hash = 'a -> int

(** Build an iterator from a iter function *)
let from_iter f = f

let from_labelled_iter iter f = iter ~f

let rec from_fun f k =
  match f () with
  | None -> ()
  | Some x ->
    k x;
    from_fun f k

let[@inline] empty _ = ()
let[@inline] return x k = k x
let singleton = return
let pure = return

let[@inline] doubleton x y k =
  k x;
  k y

let[@inline] cons x l k =
  k x;
  l k

let[@inline] snoc l x k =
  l k;
  k x

let[@inline] repeat x k =
  while true do
    k x
  done

let init f yield =
  let rec aux i =
    yield (f i);
    aux (i + 1)
  in
  aux 0

let rec iterate f x k =
  k x;
  iterate f (f x) k

let rec forever f k =
  k (f ());
  forever f k

let cycle s k =
  while true do
    s k
  done

let[@inline] iter f seq = seq f

let iteri f seq =
  let r = ref 0 in
  seq (fun x ->
      f !r x;
      incr r)

let for_each seq f = iter f seq
let for_eachi seq f = iteri f seq

let fold f init seq =
  let r = ref init in
  seq (fun elt -> r := f !r elt);
  !r

let foldi f init seq =
  let i = ref 0 in
  let r = ref init in
  seq (fun elt ->
      r := f !r !i elt;
      incr i);
  !r

let fold_map f init seq yield =
  let r = ref init in
  seq (fun x ->
      let acc', y = f !r x in
      r := acc';
      yield y)

let fold_filter_map f init seq yield =
  let r = ref init in
  seq (fun x ->
      let acc', y = f !r x in
      r := acc';
      match y with
      | None -> ()
      | Some y' -> yield y')

let[@inline] map f seq k = seq (fun x -> k (f x))

let[@inline] mapi f seq k =
  let i = ref 0 in
  seq (fun x ->
      k (f !i x);
      incr i)

let map_by_2 f seq k =
  let r = ref None in
  let f y =
    match !r with
    | None -> r := Some y
    | Some x -> k (f x y)
  in
  seq f;
  match !r with
  | None -> ()
  | Some x -> k x

let[@inline] filter p seq k = seq (fun x -> if p x then k x)

let[@inline] append s1 s2 k =
  s1 k;
  s2 k

let[@inline] append_l l k = List.iter (fun sub -> sub k) l
let[@inline] concat s k = s (fun s' -> s' k)
let flatten = concat
let[@inline] flat_map f seq k = seq (fun x -> f x k)
let[@inline] flat_map_l f seq k = seq (fun x -> List.iter k (f x))

let[@unroll 2] rec seq_list_map f l k =
  match l with
  | [] -> k []
  | x :: tail ->
    f x (fun x' -> seq_list_map f tail (fun tail' -> k (x' :: tail')))

let[@inline] seq_list l = seq_list_map (fun x -> x) l

let[@inline] filter_map f seq k =
  seq (fun x ->
      match f x with
      | None -> ()
      | Some y -> k y)

let filter_mapi f seq k =
  let i = ref 0 in
  seq (fun x ->
      let j = !i in
      incr i;
      match f j x with
      | None -> ()
      | Some y -> k y)

let filter_count f seq =
  let i = ref 0 in
  seq (fun x -> if f x then incr i);
  !i

let intersperse elem seq k =
  let first = ref true in
  seq (fun x ->
      if !first then
        first := false
      else
        k elem;
      k x)

let keep_some seq k =
  seq (function
    | Some x -> k x
    | None -> ())

let keep_ok seq k =
  seq (function
    | Result.Ok x -> k x
    | Result.Error _ -> ())

let keep_error seq k =
  seq (function
    | Result.Error x -> k x
    | Result.Ok _ -> ())

(** Mutable unrolled list to serve as intermediate storage *)
module MList = struct
  type 'a node = Nil | Cons of 'a array * int ref * 'a node ref

  (* build and call callback on every element *)
  let of_iter_with seq k =
    let start = ref Nil in
    let chunk_size = ref 8 in
    (* fill the list. prev: tail-reference from previous node *)
    let prev, cur = ref start, ref Nil in
    seq (fun x ->
        k x;
        (* callback *)
        match !cur with
        | Nil ->
          let n = !chunk_size in
          if n < 4096 then chunk_size := 2 * !chunk_size;
          cur := Cons (Array.make n x, ref 1, ref Nil)
        | Cons (a, n, next) ->
          assert (!n < Array.length a);
          a.(!n) <- x;
          incr n;
          if !n = Array.length a then (
            !prev := !cur;
            prev := next;
            cur := Nil
          ));
    !prev := !cur;
    !start

  let of_iter seq = of_iter_with seq (fun _ -> ())

  let rec iter f l =
    match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
      for i = 0 to !n - 1 do
        f a.(i)
      done;
      iter f !tl

  let iteri f l =
    let rec iteri i f l =
      match l with
      | Nil -> ()
      | Cons (a, n, tl) ->
        for j = 0 to !n - 1 do
          f (i + j) a.(j)
        done;
        iteri (i + !n) f !tl
    in
    iteri 0 f l

  let rec iter_rev f l =
    match l with
    | Nil -> ()
    | Cons (a, n, tl) ->
      iter_rev f !tl;
      for i = !n - 1 downto 0 do
        f a.(i)
      done

  let length l =
    let rec len acc l =
      match l with
      | Nil -> acc
      | Cons (_, n, tl) -> len (acc + !n) !tl
    in
    len 0 l

  (** Get element by index *)
  let rec get l i =
    match l with
    | Nil -> raise (Invalid_argument "MList.get")
    | Cons (a, n, _) when i < !n -> a.(i)
    | Cons (_, n, tl) -> get !tl (i - !n)

  let to_iter l k = iter k l

  let _to_next arg l =
    let cur = ref l in
    let i = ref 0 in
    (* offset in cons *)
    let rec get_next _ =
      match !cur with
      | Nil -> None
      | Cons (_, n, tl) when !i = !n ->
        cur := !tl;
        i := 0;
        get_next arg
      | Cons (a, _, _) ->
        let x = a.(!i) in
        incr i;
        Some x
    in
    get_next

  let to_gen l = _to_next () l

  let to_seq l =
    let rec make (l, i) () =
      match l with
      | Nil -> Seq.Nil
      | Cons (_, n, tl) when i = !n -> make (!tl, 0) ()
      | Cons (a, _, _) -> Seq.Cons (a.(i), make (l, i + 1))
    in
    make (l, 0)
end

let persistent seq =
  let l = MList.of_iter seq in
  MList.to_iter l

type 'a lazy_state = LazySuspend | LazyCached of 'a t

let persistent_lazy (seq : 'a t) =
  let r = ref LazySuspend in
  fun k ->
    match !r with
    | LazyCached seq' -> seq' k
    | LazySuspend ->
      (* here if this traversal is interruted, no caching occurs *)
      let seq' = MList.of_iter_with seq k in
      r := LazyCached (MList.to_iter seq')

let sort ?(cmp = Stdlib.compare) seq =
  (* use an intermediate list, then sort the list *)
  let l = fold (fun l x -> x :: l) [] seq in
  let l = List.fast_sort cmp l in
  fun k -> List.iter k l

exception Exit_sorted

let sorted ?(cmp = Stdlib.compare) seq =
  let prev = ref None in
  try
    seq (fun x ->
        match !prev with
        | Some y when cmp y x > 0 -> raise_notrace Exit_sorted
        | _ -> prev := Some x);
    true
  with Exit_sorted -> false

let group_succ_by ?(eq = fun x y -> x = y) seq k =
  let cur = ref [] in
  seq (fun x ->
      match !cur with
      | [] -> cur := [ x ]
      | y :: _ as l when eq x y -> cur := x :: l (* [x] belongs to the group *)
      | _ :: _ as l ->
        k l;
        (* yield group, and start another one *)
        cur := [ x ]);
  (* last list *)
  match !cur with
  | [] -> ()
  | _ :: _ as l -> k l

let group_by (type k) ?(hash = Hashtbl.hash) ?(eq = ( = )) seq =
  let module Tbl = Hashtbl.Make (struct
    type t = k

    let equal = eq
    let hash = hash
  end) in
  (* compute group table *)
  let tbl =
    lazy
      (let tbl = Tbl.create 32 in
       seq (fun x ->
           let l = try Tbl.find tbl x with Not_found -> [] in
           Tbl.replace tbl x (x :: l));
       tbl)
  in
  fun yield -> Tbl.iter (fun _ l -> yield l) (Lazy.force tbl)

let count (type k) ?(hash = Hashtbl.hash) ?(eq = ( = )) seq =
  let module Tbl = Hashtbl.Make (struct
    type t = k

    let equal = eq
    let hash = hash
  end) in
  (* compute group table *)
  let tbl =
    lazy
      (let tbl = Tbl.create 32 in
       seq (fun x ->
           let n = try Tbl.find tbl x with Not_found -> 0 in
           Tbl.replace tbl x (n + 1));
       tbl)
  in
  fun yield -> Tbl.iter (fun x n -> yield (x, n)) (Lazy.force tbl)

let uniq ?(eq = fun x y -> x = y) seq k =
  let has_prev = ref false and prev = ref (Obj.magic 0) in
  (* avoid option type, costly *)
  seq (fun x ->
      if !has_prev && eq !prev x then
        ()
      (* duplicate *)
      else (
        has_prev := true;
        prev := x;
        k x
      ))

let sort_uniq (type elt) ?(cmp = Stdlib.compare) seq =
  let module S = Set.Make (struct
    type t = elt

    let compare = cmp
  end) in
  let set = fold (fun acc x -> S.add x acc) S.empty seq in
  fun k -> S.iter k set

let[@inline] product outer inner k = outer (fun x -> inner (fun y -> k (x, y)))

let rec diagonal_l l yield =
  match l with
  | [] -> ()
  | x :: tail ->
    List.iter (fun y -> yield (x, y)) tail;
    diagonal_l tail yield

let diagonal seq =
  let l = ref [] in
  seq (fun x -> l := x :: !l);
  diagonal_l (List.rev !l)

let join ~join_row s1 s2 k =
  s1 (fun a ->
      s2 (fun b ->
          match join_row a b with
          | None -> ()
          | Some c -> k c))

let join_by (type a) ?(eq = ( = )) ?(hash = Hashtbl.hash) f1 f2 ~merge c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c1 (fun x ->
      let key = f1 x in
      Tbl.add tbl key x);
  let res = ref [] in
  c2 (fun y ->
      let key = f2 y in
      let xs = Tbl.find_all tbl key in
      List.iter
        (fun x ->
          match merge key x y with
          | None -> ()
          | Some z -> res := z :: !res)
        xs);
  fun yield -> List.iter yield !res

type ('a, 'b) join_all_cell = {
  mutable ja_left: 'a list;
  mutable ja_right: 'b list;
}

let join_all_by (type a) ?(eq = ( = )) ?(hash = Hashtbl.hash) f1 f2 ~merge c1 c2
    =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  (* build the map [key -> cell] *)
  c1 (fun x ->
      let key = f1 x in
      try
        let c = Tbl.find tbl key in
        c.ja_left <- x :: c.ja_left
      with Not_found -> Tbl.add tbl key { ja_left = [ x ]; ja_right = [] });
  c2 (fun y ->
      let key = f2 y in
      try
        let c = Tbl.find tbl key in
        c.ja_right <- y :: c.ja_right
      with Not_found -> Tbl.add tbl key { ja_left = []; ja_right = [ y ] });
  let res = ref [] in
  Tbl.iter
    (fun key cell ->
      match merge key cell.ja_left cell.ja_right with
      | None -> ()
      | Some z -> res := z :: !res)
    tbl;
  fun yield -> List.iter yield !res

let group_join_by (type a) ?(eq = ( = )) ?(hash = Hashtbl.hash) f c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c1 (fun x -> Tbl.replace tbl x []);
  c2 (fun y ->
      (* project [y] into some element of [c1] *)
      let key = f y in
      try
        let l = Tbl.find tbl key in
        Tbl.replace tbl key (y :: l)
      with Not_found -> ());
  fun yield -> Tbl.iter (fun k l -> yield (k, l)) tbl

let union (type a) ?(eq = ( = )) ?(hash = Hashtbl.hash) c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c1 (fun x -> Tbl.replace tbl x ());
  c2 (fun x -> Tbl.replace tbl x ());
  fun yield -> Tbl.iter (fun x _ -> yield x) tbl

type inter_status = Inter_left | Inter_both

let inter (type a) ?(eq = ( = )) ?(hash = Hashtbl.hash) c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c1 (fun x -> Tbl.replace tbl x Inter_left);
  c2 (fun x ->
      try
        match Tbl.find tbl x with
        | Inter_left -> Tbl.replace tbl x Inter_both
        (* save *)
        | Inter_both -> ()
      with Not_found -> ());
  fun yield -> Tbl.iter (fun x res -> if res = Inter_both then yield x) tbl

let diff (type a) ?(eq = ( = )) ?(hash = Hashtbl.hash) c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c2 (fun x -> Tbl.replace tbl x ());
  fun yield -> c1 (fun x -> if not (Tbl.mem tbl x) then yield x)

exception Subset_exit

let subset (type a) ?(eq = ( = )) ?(hash = Hashtbl.hash) c1 c2 =
  let module Tbl = Hashtbl.Make (struct
    type t = a

    let equal = eq
    let hash = hash
  end) in
  let tbl = Tbl.create 32 in
  c2 (fun x -> Tbl.replace tbl x ());
  try
    c1 (fun x -> if not (Tbl.mem tbl x) then raise_notrace Subset_exit);
    true
  with Subset_exit -> false

let rec unfoldr f b k =
  match f b with
  | None -> ()
  | Some (x, b') ->
    k x;
    unfoldr f b' k

let scan f acc seq k =
  k acc;
  let acc = ref acc in
  seq (fun elt ->
      let acc' = f !acc elt in
      k acc';
      acc := acc')

let max ?(lt = fun x y -> x < y) seq =
  let ret = ref None in
  seq (fun x ->
      match !ret with
      | None -> ret := Some x
      | Some y -> if lt y x then ret := Some x);
  !ret

let max_exn ?lt seq =
  match max ?lt seq with
  | Some x -> x
  | None -> raise_notrace Not_found

let min ?(lt = fun x y -> x < y) seq =
  let ret = ref None in
  seq (fun x ->
      match !ret with
      | None -> ret := Some x
      | Some y -> if lt x y then ret := Some x);
  !ret

let min_exn ?lt seq =
  match min ?lt seq with
  | Some x -> x
  | None -> raise Not_found

let[@inline] sum seq =
  let n = ref 0 in
  seq (fun x -> n := !n + x);
  !n

(* https://en.wikipedia.org/wiki/Kahan_summation_algorithm *)
let sumf seq : float =
  let sum = ref 0. in
  let c = ref 0. in
  (* error compensation *)
  seq (fun x ->
      let y = x -. !c in
      let t = !sum +. y in
      c := t -. !sum -. y;
      sum := t);
  !sum

exception ExitHead

let head seq =
  let r = ref None in
  try
    seq (fun x ->
        r := Some x;
        raise_notrace ExitHead);
    None
  with ExitHead -> !r

let head_exn seq =
  match head seq with
  | None -> invalid_arg "Iter.head_exn"
  | Some x -> x

exception ExitTake

let take n seq k =
  let count = ref 0 in
  try
    seq (fun x ->
        if !count = n then raise_notrace ExitTake;
        incr count;
        k x)
  with ExitTake -> ()

exception ExitTakeWhile

let take_while p seq k =
  try
    seq (fun x ->
        if p x then
          k x
        else
          raise_notrace ExitTakeWhile)
  with ExitTakeWhile -> ()

exception ExitFoldWhile

let fold_while f s seq =
  let state = ref s in
  let consume x =
    let acc, cont = f !state x in
    state := acc;
    match cont with
    | `Stop -> raise_notrace ExitFoldWhile
    | `Continue -> ()
  in
  try
    seq consume;
    !state
  with ExitFoldWhile -> !state

let drop n seq k =
  let count = ref 0 in
  seq (fun x ->
      if !count >= n then
        k x
      else
        incr count)

let drop_while p seq k =
  let drop = ref true in
  seq (fun x ->
      if !drop then
        if p x then
          ()
        else (
          drop := false;
          k x
        )
      else
        k x)

let rev seq =
  let l = MList.of_iter seq in
  fun k -> MList.iter_rev k l

exception ExitForall

let for_all p seq =
  try
    seq (fun x -> if not (p x) then raise_notrace ExitForall);
    true
  with ExitForall -> false

exception ExitExists

(** Exists there some element satisfying the predicate? *)
let exists p seq =
  try
    seq (fun x -> if p x then raise_notrace ExitExists);
    false
  with ExitExists -> true

let mem ?(eq = ( = )) x seq = exists (eq x) seq

exception ExitFind

let find_map f seq =
  let r = ref None in
  (try
     seq (fun x ->
         match f x with
         | None -> ()
         | Some _ as res ->
           r := res;
           raise_notrace ExitFind)
   with ExitFind -> ());
  !r

let find = find_map

let find_mapi f seq =
  let i = ref 0 in
  let r = ref None in
  (try
     seq (fun x ->
         match f !i x with
         | None -> incr i
         | Some _ as res ->
           r := res;
           raise_notrace ExitFind)
   with ExitFind -> ());
  !r

let findi = find_mapi

let find_pred f seq =
  find_map
    (fun x ->
      if f x then
        Some x
      else
        None)
    seq

let find_pred_exn f seq =
  match find_pred f seq with
  | Some x -> x
  | None -> raise Not_found

let[@inline] length seq =
  let r = ref 0 in
  seq (fun _ -> incr r);
  !r

exception ExitIsEmpty

let is_empty seq =
  try
    seq (fun _ -> raise_notrace ExitIsEmpty);
    true
  with ExitIsEmpty -> false

(** {2 Transform an iterator} *)

let[@inline] zip_i seq k =
  let r = ref 0 in
  seq (fun x ->
      let n = !r in
      incr r;
      k (n, x))

let fold2 f acc seq2 =
  let acc = ref acc in
  seq2 (fun (x, y) -> acc := f !acc x y);
  !acc

let[@inline] iter2 f seq2 = seq2 (fun (x, y) -> f x y)
let[@inline] map2 f seq2 k = seq2 (fun (x, y) -> k (f x y))
let[@inline] map2_2 f g seq2 k = seq2 (fun (x, y) -> k (f x y, g x y))

(** {2 Basic data structures converters} *)

let to_list seq = List.rev (fold (fun y x -> x :: y) [] seq)
let[@inline] to_rev_list seq = fold (fun y x -> x :: y) [] seq
let[@inline] of_list l k = List.iter k l
let on_list f l = to_list (f (of_list l))

let pair_with_idx seq k =
  let r = ref 0 in
  seq (fun x ->
      let n = !r in
      incr r;
      k (n, x))

let to_opt = head

let[@inline] of_opt o k =
  match o with
  | None -> ()
  | Some x -> k x

let to_array seq =
  let l = MList.of_iter seq in
  let n = MList.length l in
  if n = 0 then
    [||]
  else (
    let a = Array.make n (MList.get l 0) in
    MList.iteri (fun i x -> a.(i) <- x) l;
    a
  )

let[@inline] of_array a k = Array.iter k a

let[@inline] of_array_i a k =
  for i = 0 to Array.length a - 1 do
    k (i, Array.unsafe_get a i)
  done

let array_slice a i j k =
  assert (i >= 0 && j < Array.length a);
  for idx = i to j do
    k a.(idx) (* iterate on sub-array *)
  done

let rec of_seq l k =
  match l () with
  | Seq.Nil -> ()
  | Seq.Cons (x, tl) ->
    k x;
    of_seq tl k

let to_seq_persistent seq =
  let l = MList.of_iter seq in
  MList.to_seq l

let[@inline] to_stack s seq = iter (fun x -> Stack.push x s) seq
let[@inline] of_stack s k = Stack.iter k s
let[@inline] to_queue q seq = seq (fun x -> Queue.push x q)
let[@inline] of_queue q k = Queue.iter k q
let[@inline] hashtbl_add h seq = seq (fun (k, v) -> Hashtbl.add h k v)
let hashtbl_replace h seq = seq (fun (k, v) -> Hashtbl.replace h k v)

let to_hashtbl seq =
  let h = Hashtbl.create 3 in
  hashtbl_replace h seq;
  h

let[@inline] of_hashtbl h k = Hashtbl.iter (fun a b -> k (a, b)) h
let hashtbl_keys h k = Hashtbl.iter (fun a _ -> k a) h
let hashtbl_values h k = Hashtbl.iter (fun _ b -> k b) h
let[@inline] of_str s k = String.iter k s

let to_str seq =
  let b = Buffer.create 64 in
  iter (fun c -> Buffer.add_char b c) seq;
  Buffer.contents b

let concat_str seq =
  let b = Buffer.create 64 in
  iter (Buffer.add_string b) seq;
  Buffer.contents b

exception OneShotSequence

let of_in_channel ic =
  let first = ref true in
  fun k ->
    if not !first then
      raise OneShotSequence
    else (
      first := false;
      try
        while true do
          let c = input_char ic in
          k c
        done
      with End_of_file -> ()
    )

let to_buffer seq buf = seq (fun c -> Buffer.add_char buf c)

(** Iterator on integers in [start...stop] by steps 1 *)
let int_range ~start ~stop k =
  for i = start to stop do
    k i
  done

let int_range_dec ~start ~stop k =
  for i = start downto stop do
    k i
  done

let int_range_by ~step i j yield =
  if step = 0 then invalid_arg "int_range_by";
  for k = 0 to (j - i) / step do
    yield ((k * step) + i)
  done

let bools k =
  k false;
  k true

let of_set (type s v) m set =
  let module S = (val m : Set.S with type t = s and type elt = v) in
  fun k -> S.iter k set

let to_set (type s v) m seq =
  let module S = (val m : Set.S with type t = s and type elt = v) in
  fold (fun set x -> S.add x set) S.empty seq

type 'a gen = unit -> 'a option

(* consume the generator to build a MList *)
let rec of_gen1_ g k =
  match g () with
  | None -> ()
  | Some x ->
    k x;
    of_gen1_ g k

let of_gen_once g =
  let first = ref true in
  fun k ->
    if !first then
      first := false
    else
      raise OneShotSequence;
    of_gen1_ g k

let of_gen g =
  let l = MList.of_iter (of_gen1_ g) in
  MList.to_iter l

let to_gen seq =
  let l = MList.of_iter seq in
  MList.to_gen l

(** {2 Functorial conversions between sets and iterators} *)

module Set = struct
  module type S = sig
    include Set.S

    val of_iter : elt iter -> t
    val to_iter : t -> elt iter
    val to_list : t -> elt list
    val of_list : elt list -> t

    val of_seq : elt iter -> t
    (** @deprecated use {!of_iter} instead *)

    val to_seq : t -> elt iter
    (** @deprecated use {!to_iter} instead *)
  end

  (** Create an enriched Set module from the given one *)
  module Adapt (X : Set.S) : S with type elt = X.elt and type t = X.t = struct
    let to_iter_ set k = X.iter k set
    let of_iter_ seq = fold (fun set x -> X.add x set) X.empty seq

    include X

    let to_iter = to_iter_
    let of_iter = of_iter_
    let to_seq = to_iter_
    let of_seq = of_iter_
    let of_list l = List.fold_left (fun set x -> add x set) empty l
    let to_list = elements
  end

  (** Functor to build an extended Set module from an ordered type *)
  module Make (X : Set.OrderedType) = struct
    module MySet = Set.Make (X)
    include Adapt (MySet)
  end
end

(** {2 Conversion between maps and iterators.} *)

module Map = struct
  module type S = sig
    include Map.S

    val to_iter : 'a t -> (key * 'a) iter
    val of_iter : (key * 'a) iter -> 'a t
    val keys : 'a t -> key iter
    val values : 'a t -> 'a iter
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t

    val to_seq : 'a t -> (key * 'a) iter
    (** @deprecated use {!to_iter} instead *)

    val of_seq : (key * 'a) iter -> 'a t
    (** @deprecated use {!of_iter} instead *)
  end

  (** Adapt a pre-existing Map module to make it iterator-aware *)
  module Adapt (M : Map.S) = struct
    let to_iter_ m = from_iter (fun k -> M.iter (fun x y -> k (x, y)) m)
    let of_iter_ seq = fold (fun m (k, v) -> M.add k v m) M.empty seq
    let keys m = from_iter (fun k -> M.iter (fun x _ -> k x) m)
    let values m = from_iter (fun k -> M.iter (fun _ y -> k y) m)
    let of_list l = of_iter_ (of_list l)
    let to_list x = to_list (to_iter_ x)

    include M

    let to_iter = to_iter_
    let of_iter = of_iter_
    let to_seq = to_iter_
    let of_seq = of_iter_
  end

  (** Create an enriched Map module, with iterator-aware functions *)
  module Make (V : Map.OrderedType) : S with type key = V.t = struct
    module M = Map.Make (V)
    include Adapt (M)
  end
end

(** {2 Infinite iterators of random values} *)

let random_int bound = forever (fun () -> Random.int bound)
let random_bool = forever Random.bool
let random_float bound = forever (fun () -> Random.float bound)

let random_array a k =
  assert (Array.length a > 0);
  while true do
    let i = Random.int (Array.length a) in
    k a.(i)
  done

let random_list l = random_array (Array.of_list l)

(* See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle *)
let shuffle_array a =
  for k = Array.length a - 1 downto 0 + 1 do
    let l = Random.int (k + 1) in
    let tmp = a.(l) in
    a.(l) <- a.(k);
    a.(k) <- tmp
  done

let shuffle seq =
  let a = to_array seq in
  shuffle_array a;
  of_array a

let shuffle_buffer n seq k =
  let seq_front = take n seq in
  let a = to_array seq_front in
  let l = Array.length a in
  if l < n then (
    shuffle_array a;
    of_array a k
  ) else (
    let seq = drop n seq in
    let f x =
      let i = Random.int n in
      let y = a.(i) in
      a.(i) <- x;
      k y
    in
    seq f
  )

(** {2 Sampling} *)

(** See https://en.wikipedia.org/wiki/Reservoir_sampling#Algorithm_R *)
let sample k seq =
  match head seq with
  | None -> [||]
  | Some x ->
    let a = Array.make k x in
    let i = ref (-1) in
    let f x =
      incr i;
      if !i < k then
        a.(!i) <- x
      else (
        let j = Random.int !i in
        if j < k then
          a.(j) <- x
        else
          ()
      )
    in
    seq f;
    if !i < k then
      Array.sub a 0 (!i + 1)
    else
      a

(** {2 Infix functions} *)

module Infix = struct
  let[@inline] ( -- ) i j = int_range ~start:i ~stop:j
  let[@inline] ( --^ ) i j = int_range_dec ~start:i ~stop:j
  let[@inline] ( >>= ) x f = flat_map f x
  let[@inline] ( >|= ) x f = map f x
  let[@inline] ( <*> ) funs args k = funs (fun f -> args (fun x -> k (f x)))
  let ( <+> ) = append
end

include Infix

(** {2 Pretty printing of iterators} *)

(** Pretty print an ['a iter], using the given pretty printer
    to print each elements. An optional separator string can be provided. *)
let pp_seq ?(sep = ", ") pp_elt formatter seq =
  let first = ref true in
  seq (fun x ->
      if !first then
        first := false
      else (
        Format.pp_print_string formatter sep;
        Format.pp_print_cut formatter ()
      );
      pp_elt formatter x)

let pp_buf ?(sep = ", ") pp_elt buf seq =
  let first = ref true in
  seq (fun x ->
      if !first then
        first := false
      else
        Buffer.add_string buf sep;
      pp_elt buf x)

let to_string ?sep pp_elt seq =
  let buf = Buffer.create 25 in
  pp_buf ?sep (fun buf x -> Buffer.add_string buf (pp_elt x)) buf seq;
  Buffer.contents buf

(** {2 Basic IO} *)

module IO = struct
  let lines_of ?(mode = 0o644) ?(flags = [ Open_rdonly ]) filename k =
    let ic = open_in_gen flags mode filename in
    try
      while true do
        let line = input_line ic in
        k line
      done
    with
    | End_of_file -> close_in ic
    | e ->
      close_in_noerr ic;
      raise e

  let chunks_of ?(mode = 0o644) ?(flags = []) ?(size = 1024) filename k =
    let ic = open_in_gen flags mode filename in
    try
      let buf = Bytes.create size in
      let n = ref 0 in
      let stop = ref false in
      while not !stop do
        n := 0;
        (* try to read [size] chars. If [input] returns [0] it means
            the end of file, so we stop, but first we yield the current chunk *)
        while !n < size && not !stop do
          let n' = input ic buf !n (size - !n) in
          if n' = 0 then
            stop := true
          else
            n := !n + n'
        done;
        if !n > 0 then k (Bytes.sub_string buf 0 !n)
      done;
      close_in ic
    with e ->
      close_in_noerr ic;
      raise e

  let with_out_ ?(mode = 0o644) ?(flags = [ Open_creat; Open_wronly ]) filename
      f =
    let oc = open_out_gen flags mode filename in
    try
      f oc;
      close_out oc
    with e ->
      close_out oc;
      raise e

  let write_bytes_to ?mode ?flags filename it =
    with_out_ ?mode ?flags filename (fun oc ->
        it (fun s -> output oc s 0 (Bytes.length s)))

  let write_to ?mode ?flags filename seq =
    write_bytes_to ?mode ?flags filename (map Bytes.unsafe_of_string seq)

  let write_bytes_lines ?mode ?flags filename it =
    with_out_ ?mode ?flags filename (fun oc ->
        it (fun s ->
            output oc s 0 (Bytes.length s);
            output_char oc '\n'))

  let write_lines ?mode ?flags filename seq =
    write_bytes_lines ?mode ?flags filename (map Bytes.unsafe_of_string seq)
end
