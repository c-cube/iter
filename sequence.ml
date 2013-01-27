
(** {2 Transient iterators, that abstract on a finite sequence of elements. *)

(** Sequence abstract iterator type *)
type 'a sequence = {
  seq_fun: ('a -> unit) -> unit;
}

(** Build a sequence from a iter function *)
let from_iter f = {
  seq_fun = f;
}

(** Consume the sequence, passing all its arguments to the function *)
let iter f seq = seq.seq_fun f

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

(** Concatenate two sequences *)
let concat s1 s2 =
  let seq_fun k = s1.seq_fun k; s2.seq_fun k in
  { seq_fun; }

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

