
(** {2 Transient iterators, that abstract on a finite sequence of elements. *)

type 'a t
  (** Sequence abstract iterator type *)

(** {2 Build a sequence} *)

val from_iter : (('a -> unit) -> unit) -> 'a t
  (** Build a sequence from a iter function *)

(** {2 Use a sequence} *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Consume the sequence, passing all its arguments to the function *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** Iterate on elements and their index in the sequence *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold over elements of the sequence, consuming it *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map objects of the sequence into other elements, lazily *)

val filter : ('a -> bool) -> 'a t -> 'a t
  (** Filter on elements of the sequence *)

val append : 'a t -> 'a t -> 'a t
  (** Append two sequences *)

val concat : 'a t t -> 'a t
  (** Concatenate a sequence of sequences into one sequence *)

val take : int -> 'a t -> 'a t
  (** Take at most [n] elements from the sequence *)

val drop : int -> 'a t -> 'a t
  (** Drop the [n] first elements of the sequence *)

val rev : 'a t -> 'a t
  (** Reverse the sequence. O(n) memory. *)

val for_all : ('a -> bool) -> 'a t -> bool
  (** Do all elements satisfy the predicate? *)

val exists : ('a -> bool) -> 'a t -> bool
  (** Exists there some element satisfying the predicate? *)

(** {2 Basic data structures converters} *)

module List :
  sig
    val of_seq : 'a t -> 'a list
    val to_seq : 'a list -> 'a t
  end

module Hashtbl :
  sig
    val of_seq : ('a * 'b) t -> ('a, 'b) Hashtbl.t
    val to_seq : ('a, 'b) Hashtbl.t -> ('a * 'b) t
  end

(** Iterate on ranges of ints *)
module Int :
  sig
    val range : start:int -> stop:int -> int t
      (** Iterator on [start...stop] by steps 1 *)
  end

