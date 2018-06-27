
(* This file is free software, part of sequence. See file "license" for more details. *)

(** {1 Interface and Helpers for bigarrays}

@since 0.5.4 *)

val of_bigarray : ('a, _, _) Bigarray.Array1.t -> 'a Sequence.t
(** Iterate on the elements of a 1-D array *)

val mmap : string -> char Sequence.t
(** Map the file into memory, and read the characters. *)
