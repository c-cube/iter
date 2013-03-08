
open OUnit

module S = Sequence

let test_empty () =
  let seq = S.empty in
  OUnit.assert_bool "empty" (S.is_empty seq);
  OUnit.assert_bool "empty"
    (try S.iter (fun _ -> raise Exit) seq; true with Exit -> false);
  ()

let suite =
  "test_sequence" >:::
    [ "test_empty" >:: test_empty;
      
    ]
