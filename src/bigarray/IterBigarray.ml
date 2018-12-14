
(* This file is free software, part of sequence. See file "license" for more details. *)

(** {1 Interface and Helpers for bigarrays} *)

open! Bigarray

let of_bigarray b yield =
  let len = Bigarray.Array1.dim b in
  for i=0 to len-1 do
    yield b.{i}
  done

let mmap filename =
  fun yield ->
    let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
    let len = Unix.lseek fd 0 Unix.SEEK_END in
    let _ = Unix.lseek fd 0 Unix.SEEK_SET in
    let b = Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout false len in
    try
      of_bigarray b yield;
      Unix.close fd
    with e ->
      Unix.close fd;
      raise e
[@@ocaml.warning "-3"]
