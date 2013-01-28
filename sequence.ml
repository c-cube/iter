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
type 'a t = {
  seq_fun: ('a -> unit) -> unit;
}

(** Build a sequence from a iter function *)
let from_iter f = {
  seq_fun = f;
}

let singleton x = {
  seq_fun = fun k -> k x;
}

(** Consume the sequence, passing all its arguments to the function *)
let iter f seq = seq.seq_fun f

(** Iterate on elements and their index in the sequence *)
let iteri f seq =
  let r = ref 0 in
  let k x =
    f !r x;
    incr r
  in seq.seq_fun k

(** Fold over elements of the sequence, consuming it *)
let fold f init seq =
  let r = ref init in
  seq.seq_fun (fun elt -> r := f !r elt);
  !r
    
(** Map objects of the sequence into other elements, lazily *)
let map f seq =
  let seq_fun' k = seq.seq_fun (fun x -> k (f x)) in
  { seq_fun=seq_fun'; }

(** Filter on elements of the sequence *)
let filter p seq =
  let seq_fun' k = seq.seq_fun (fun x -> if p x then k x) in
  { seq_fun=seq_fun'; }

(** Append two sequences *)
let append s1 s2 =
  let seq_fun k = s1.seq_fun k; s2.seq_fun k in
  { seq_fun; }

(** Concatenate a sequence of sequences into one sequence *)
let concat s =
  let seq_fun k =
    (* function that is called on every sub-sequence *)
    let k_seq seq = iter k seq in
    s.seq_fun k_seq
  in { seq_fun; }

(** Take at most [n] elements from the sequence *)
let take n seq =
  let count = ref 0 in
  let seq_fun k = seq.seq_fun
    (fun x ->
      if !count < n then begin incr count; k x end)
  in { seq_fun; }

(** Drop the [n] first elements of the sequence *)
let drop n seq =
  let count = ref 0 in
  let seq_fun k = seq.seq_fun
    (fun x -> if !count >= n then k x else incr count)
  in { seq_fun; }

(** Reverse the sequence. O(n) memory. *)
let rev seq =
  let seq_fun k =
    (* if we have traversed [s_1, ..., s_m], [cont ()] will call [k] on s_m,
       s_{m-1}, ..., s_1. Once we know [s_{m+1}], we update [cont] so that it
       first returns it, and then called the previous cont. *)
    let cont = ref (fun () -> ()) in
    iter (fun x ->
      let current_cont = !cont in
      let cont' () = k x; current_cont () in
      cont := cont') seq;
    !cont ()
  in { seq_fun; }

(** Do all elements satisfy the predicate? *)
let for_all p seq =
  try
    seq.seq_fun (fun x -> if not (p x) then raise Exit);
    true
  with Exit -> false

(** Exists there some element satisfying the predicate? *)
let exists p seq =
  try
    seq.seq_fun (fun x -> if p x then raise Exit);
    false
  with Exit -> true

module List =
  struct
    let of_seq seq = List.rev (fold (fun y x -> x::y) [] seq)
    let to_seq l = from_iter (fun k -> List.iter k l)
  end

module Hashtbl =
  struct
    let of_seq seq =
      let h = Hashtbl.create 3 in
      iter (fun (k,v) -> Hashtbl.replace h k v) seq;
      h
    let to_seq h =
      from_iter (fun k -> Hashtbl.iter (fun a b -> k (a, b)) h)
  end

module Int =
  struct
    let range ~start ~stop =
      let seq_fun k =
        for i = start to stop do k i done
      in { seq_fun; }
  end

module Set =
  struct
    let to_seq (type s) (type t) m set =
      let module S = (val m : Set.S with type elt = s and type t = t) in
      from_iter (fun k -> S.iter k set)

    let of_seq (type s) (type t) m seq =
      let module S = (val m : Set.S with type elt = s and type t = t) in
      fold (fun set x -> S.add x set) S.empty seq
  end
