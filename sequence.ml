
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
