
module S = Sequence
open Sequence.Infix

let small = [10;20;50;100;500]
let medium = small @ [1000;10_000;100_000]
let big = medium @ [500_000; 1_000_000; 2_000_000]

let bench_fold n =
  0 -- n |> S.fold (+) 0 |> ignore

let bench_flatmap n =
  0 -- n |> S.flatMap (fun i -> i -- (i+5)) |> (fun _ -> ())

let bench_product n =
  S.product (0 -- n) (0 -- n) (fun (i,j) -> ())

let _ =
  let _ = List.map
    (fun (name,bench) ->
      Format.printf "-------------------------------------------------------@.";
      Format.printf "bench %s@." name;
      bench ();)
    [ "fold", (fun () -> Bench.bench_throughput bench_fold big) ;
      "flatmap", (fun () -> Bench.bench_throughput bench_flatmap medium) ;
      "product", (fun () -> Bench.bench_throughput bench_product small) ;
    ]
  in
  ()
