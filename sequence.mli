
(** {2 Transient iterators, that abstract on a finite sequence of elements. *)

type 'a sequence
  (** Sequence abstract iterator type *)

(** {2 Build a sequence} *)

val from_iter : (('a -> unit) -> unit) -> 'a sequence
  (** Build a sequence from a iter function *)

(** {2 Use a sequence} *)

val iter : ('a -> unit) -> 'a sequence -> unit
  (** Consume the sequence, passing all its arguments to the function *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a sequence -> 'b
  (** Fold over elements of the sequence, consuming it *)

val map : ('a -> 'b) -> 'a sequence -> 'b sequence
  (** Map objects of the sequence into other elements, lazily *)

val filter : ('a -> bool) -> 'a sequence -> 'a sequence
  (** Filter on elements of the sequence *)

val concat : 'a sequence -> 'a sequence -> 'a sequence
  (** Concatenate two sequences *)

val take : int -> 'a sequence -> 'a sequence
  (** Take at most [n] elements from the sequence *)

val drop : int -> 'a sequence -> 'a sequence
  (** Drop the [n] first elements of the sequence *)

(** {2 Basic data structures converters} *)

module List :
  sig
    val of_seq : 'a sequence -> 'a list
    val to_seq : 'a list -> 'a sequence
  end

module Hashtbl :
  sig
    val of_seq : ('a * 'b) sequence -> ('a, 'b) Hashtbl.t
    val to_seq : ('a, 'b) Hashtbl.t -> ('a * 'b) sequence
  end
