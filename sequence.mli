(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
  damages (including, but not limited to, procurement of substitute goods or
  services; loss of use, data, or profits; or business interruption) however
  caused and on any theory of liability, whether in contract, strict liability,
  or tort (including negligence or otherwise) arising in any way out of the use
  of this software, even if advised of the possibility of such damage.
*)

(** Transient iterators, that abstract on a finite sequence of elements. They
    are designed to allow easy transfer (mappings) between data structures,
    without defining n^2 conversions between the n types. *)

type 'a t
  (** Sequence abstract iterator type, representing a finite sequence of
      values of type ['a]. *)

(** {2 Build a sequence} *)

val from_iter : (('a -> unit) -> unit) -> 'a t
  (** Build a sequence from a iter function *)

val singleton : 'a -> 'a t
  (** Singleton sequence *)

(** {2 Consume a sequence} *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Consume the sequence, passing all its arguments to the function *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** Iterate on elements and their index in the sequence *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  (** Fold over elements of the sequence, consuming it *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map objects of the sequence into other elements, lazily *)

val for_all : ('a -> bool) -> 'a t -> bool
  (** Do all elements satisfy the predicate? *)

val exists : ('a -> bool) -> 'a t -> bool
  (** Exists there some element satisfying the predicate? *)

(** {2 Transform a sequence} *)

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

(** {2 Basic data structures converters} *)

module List :
  sig
    val of_seq : 'a t -> 'a list

    val to_seq : 'a list -> 'a t
  end

module Array :
  sig
    val of_seq : 'a t -> 'a array

    val to_seq : 'a array -> 'a t

    val slice : 'a array -> int -> int -> 'a t
      (** [slice a i j] Sequence of elements whose indexes range
          from [i] to [j] *)
  end

module Stack :
  sig
    val push_seq : 'a Stack.t -> 'a t -> unit
      (** Push elements of the sequence on the stack *)

    val to_seq : 'a Stack.t -> 'a t
      (** Sequence of elements of the stack (same order as [Stack.iter]) *)
  end

module Queue :
  sig
    val push_seq : 'a Queue.t -> 'a t -> unit
      (** Push elements of the sequence into the queue *)

    val to_seq : 'a Queue.t -> 'a t
      (** Sequence of elements contained in the queue, FIFO order *)
  end

module Hashtbl :
  sig
    val add_seq : ('a, 'b) Hashtbl.t -> ('a * 'b) t -> unit
      (** Add elements of the sequence to the hashtable, with
          Hashtbl.add *)

    val replace_seq : ('a, 'b) Hashtbl.t -> ('a * 'b) t -> unit
      (** Add elements of the sequence to the hashtable, with
          Hashtbl.replace (erases conflicting bindings) *)

    val of_seq : ('a * 'b) t -> ('a, 'b) Hashtbl.t
      (** Build a hashtable from a sequence *)

    val to_seq : ('a, 'b) Hashtbl.t -> ('a * 'b) t
      (** Sequence of key/value pairs from the hashtable *)
  end

(** Sequences of ints *)
module Int :
  sig
    val range : start:int -> stop:int -> int t
      (** Iterator on [start...stop] by steps 1 *)

    val repeat : int -> int t
      (** Infinite sequence of integers. Should be used only with 
          transformers such as [take], that work even with infinite
          sequences. *)
  end

(** Iterate on sets. The Set module has to be provided. *)
module Set :
  sig
    val to_seq : (module Set.S with type elt = 'a and type t = 'b) -> 'b -> 'a t
    val of_seq : (module Set.S with type elt = 'a and type t = 'b) -> 'a t -> 'b
  end
