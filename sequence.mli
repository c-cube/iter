
(** {2 Transient iterators, that abstract on a finite sequence of elements. *)

type 'a sequence
  (** Sequence abstract iterator type *)

val from_iter : (('a -> unit) -> unit) -> 'a sequence
  (** Build a sequence from a iter function *)

val iter : ('a -> unit) -> 'a sequence -> unit
  (** Consume the sequence, passing all its arguments to the function *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a sequence -> 'b
  (** Fold over elements of the sequence, consuming it *)

val map : ('a -> 'b) -> 'a sequence -> 'b sequence
  (** Map objects of the sequence into other elements, lazily *)

val filter : ('a -> bool) -> 'a sequence -> 'a sequence
  (** Filter on elements of the sequence *)
