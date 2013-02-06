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

(** {2 Transient iterators, that abstract on a finite sequence of elements. *)

(** Sequence abstract iterator type *)
type 'a t = ('a -> unit) -> unit

(** Build a sequence from a iter function *)
let from_iter f = f

let singleton x = fun k -> k x

(** Infinite sequence of the same element *)
let repeat x = fun k -> while true do k x done

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
    
(** Map objects of the sequence into other elements, lazily *)
let map f seq =
  let seq_fun' k = seq (fun x -> k (f x)) in
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
  fun k ->
    (* function that is called on every sub-sequence *)
    let k_seq seq = iter k seq in
    s k_seq

exception ExitSequence

(** Take at most [n] elements from the sequence *)
let take n seq =
  let count = ref 0 in
  fun k ->
    try
      seq
        (fun x -> if !count < n then begin incr count; k x end
                                else raise ExitSequence)
    with ExitSequence -> ()

(** Drop the [n] first elements of the sequence *)
let drop n seq =
  let count = ref 0 in
  fun k -> seq
    (fun x -> if !count >= n then k x else incr count)

(** Reverse the sequence. O(n) memory. *)
let rev seq =
  fun k ->
    (* if we have traversed [s_1, ..., s_m], [cont ()] will call [k] on s_m,
       s_{m-1}, ..., s_1. Once we know [s_{m+1}], we update [cont] so that it
       first returns it, and then called the previous cont. *)
    let cont = ref (fun () -> ()) in
    iter (fun x ->
      let current_cont = !cont in
      let cont' () = k x; current_cont () in
      cont := cont') seq;
    !cont ()

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

let to_list seq = List.rev (fold (fun y x -> x::y) [] seq)

let to_rev_list seq = fold (fun y x -> x :: y) [] seq
  (** Get the list of the reversed sequence (more efficient) *)

let of_list l = from_iter (fun k -> List.iter k l)

let to_array seq =
  (* intermediate list... *)
  let l = to_rev_list seq in
  let a = Array.of_list l in
  (* reverse array *)
  let n = Array.length a in
  for i = 0 to (n-1) / 2 do
    let tmp = a.(i) in
    a.(i) <- a.(n-i-1);
    a.(n-i-1) <- tmp;
  done;
  a

let of_array a = from_iter (fun k -> Array.iter k a)

(** [array_slice a i j] Sequence of elements whose indexes range
    from [i] to [j] *)
let array_slice a i j =
  assert (i >= 0 && j < Array.length a);
  fun k ->
    for idx = i to j do
      k a.(idx);  (* iterate on sub-array *)
    done

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

let of_hashtbl h =
  from_iter (fun k -> Hashtbl.iter (fun a b -> k (a, b)) h)

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

(** Iterator on integers in [start...stop] by steps 1 *)
let int_range ~start ~stop =
  fun k ->
    for i = start to stop do k i done

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

(** Iterate on maps. The functor must be instantiated with a map type *)
module Map(M : Map.S) =
  struct
    type 'a map = 'a M.t
    type key = M.key
    
    let to_seq m =
      from_iter (fun k -> M.iter (fun key value -> k (key, value)) m)

    let keys m =
      from_iter (fun k -> M.iter (fun key _ -> k key) m)

    let values m =
      from_iter (fun k -> M.iter (fun _ value -> k value) m)

    let of_seq seq =
      fold (fun m (key,value) -> M.add key value m) M.empty seq
  end

(** {2 Pretty printing of sequences} *)

(** Pretty print a sequence of ['a], using the given pretty printer
    to print each elements. An optional separator string can be provided. *)
let pp_seq ?(sep=", ") pp_elt formatter seq =
  let first = ref true in
  iter
    (fun x -> 
      (if !first then first := false else Format.pp_print_string formatter sep);
      pp_elt formatter x;
      Format.pp_print_cut formatter ())
    seq
