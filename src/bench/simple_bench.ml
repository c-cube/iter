
open Iter.Infix

let _ =
  let n = int_of_string Sys.argv.(1) in
  let seq = 0 -- n in
  let start = Unix.gettimeofday () in
  seq |> Iter.persistent |> Iter.fold (+) 0 |> ignore;
  let stop = Unix.gettimeofday () in
  Format.printf "iter on %d: %.4f@." n (stop -. start);
  ()
