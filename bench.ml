open ExtLib

(* sum of the ints in the list *)
let sum_list_seq l =
  Sequence.fold (+) 0 (Sequence.of_list l)

(* sum of the ints in the list *)
let sum_list_enum l =
  Enum.fold (+) 0 (List.enum l)

(* force the list *)
let force_list_seq l =
  Sequence.persistent (Sequence.of_list l)

let force_list_enum l =
  let enum = List.enum l in
  Enum.force enum;
  enum

let make_list n =
  Sequence.to_list (Sequence.int_range ~start:0 ~stop:n)

let measure_time f x =
  let start = Unix.gettimeofday () in
  let y = f x in
  let stop = Unix.gettimeofday () in
  y, stop -. start

let _ =
  let n = int_of_string Sys.argv.(1) in
  Format.printf "compare sum on lists of size %d@." n;
  let l = make_list n in
  Format.printf "list created, length %d@." (List.length l);
  let x1, time_seq = measure_time sum_list_seq l in
  let x2, time_enum = measure_time sum_list_enum l in
  assert (x1 = x2);
  Format.printf "time for fold: seq: %.3f, enum: %.3f@." time_seq time_enum;
  let x1, time_seq = measure_time force_list_seq l in
  let x2, time_enum = measure_time force_list_enum l in
  Format.printf "%d, %d@." (Sequence.length x1) (Enum.count x2);
  assert (Sequence.length x1 = Enum.count x2);
  Format.printf "time for force: seq: %.3f, enum: %.3f@." time_seq time_enum;
  ()

(* ocamlfind ocamlopt -I `ocamlfind query sequence` -I `ocamlfind query extlib`
  sequence.cmxa extLib.cmxa unix.cmxa bench.ml -o bench *)
