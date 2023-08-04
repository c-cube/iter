module Q = QCheck
open Iter

let spf = Printf.sprintf
let pp_ilist = Q.Print.(list int)
let qchecks = ref []

let add_qcheck line gen prop =
  let test = Q.Test.make gen prop ~name:(spf "qcheck %d" line) in
  qchecks := test :: !qchecks

let () =
  let seq = empty in
  OUnit.assert_bool "empty" (is_empty seq);
  OUnit.assert_bool "empty"
    (try
       iter (fun _ -> raise Exit) seq;
       true
     with Exit -> false)

let () =
  let seq = repeat "hello" in
  OUnit.assert_equal [ "hello"; "hello"; "hello" ] (seq |> take 3 |> to_list)

let () =
  OUnit.assert_equal [ 0; 1; 2; 3; 4 ] (init (fun x -> x) |> take 5 |> to_list)

let () =
  let n = 1 -- 10 |> fold ( + ) 0 in
  OUnit.assert_equal 55 n;
  ()

let () =
  let l =
    [ "hello"; "world" ] |> of_list |> foldi (fun acc i x -> (i, x) :: acc) []
  in
  OUnit.assert_equal [ 1, "world"; 0, "hello" ] l;
  ()

let () =
  OUnit.assert_equal
    ~printer:Q.Print.(list int)
    [ 0; 1; 3; 5 ]
    (0 -- 3 |> fold_map (fun prev x -> x, prev + x) 0 |> to_list)

let () =
  let s1 = 1 -- 5 in
  let s2 = 6 -- 10 in
  let l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  OUnit.assert_equal l (to_list (append s1 s2));
  ()

let () =
  1 -- 1000
  |> map (fun i -> i -- (i + 1))
  |> concat |> length |> OUnit.assert_equal 2000

let () =
  1 -- 1000
  |> flat_map (fun i -> i -- (i + 1))
  |> length |> OUnit.assert_equal 2000

let test = OUnit.assert_equal ~printer:Q.Print.(list @@ list int)

let () =
  test
    [ [ 1; 2 ]; [ 1; 3 ] ]
    (seq_list [ singleton 1; doubleton 2 3 ] |> to_list)

let () = test [] (seq_list [ singleton 1; empty; doubleton 2 3 ] |> to_list)

let () =
  test
    [ [ 1; 2; 4 ]; [ 1; 3; 4 ] ]
    (seq_list [ singleton 1; doubleton 2 3; singleton 4 ] |> to_list)

let () =
  add_qcheck __LINE__
    Q.(list int)
    (fun l ->
      let seq = of_list l and f x = x mod 2 = 0 in
      filter_count f seq = (filter f seq |> length))

let () =
  1 -- 100
  |> (fun seq -> intersperse 0 seq)
  |> take 10 |> to_list
  |> OUnit.assert_equal [ 1; 0; 2; 0; 3; 0; 4; 0; 5; 0 ]

let () =
  let printer = pp_ilist in
  let iter =
    of_gen_once
      (let i = ref (-1) in
       fun () ->
         incr i;
         if !i < 5 then
           Some !i
         else
           None)
  in
  (* consume iter into a persistent version of itself *)
  let iter' = persistent iter in
  OUnit.assert_raises OneShotSequence (fun () -> iter |> to_list);
  OUnit.assert_equal ~printer [ 0; 1; 2; 3; 4 ] (iter' |> to_list);
  OUnit.assert_equal ~printer [ 0; 1; 2; 3; 4 ] (iter' |> to_list);
  OUnit.assert_equal ~printer [ 0; 1; 2; 3; 4 ]
    (iter' |> to_seq_persistent |> of_seq |> to_list);
  ()

let () =
  let printer = pp_ilist in
  let iter = 0 -- 10_000 in
  let iter' = persistent iter in
  OUnit.assert_equal 10_001 (length iter');
  OUnit.assert_equal 10_001 (length iter');
  OUnit.assert_equal ~printer [ 0; 1; 2; 3 ] (iter' |> take 4 |> to_list);
  ()

let () =
  1 -- 100
  |> sort ~cmp:(fun i j -> j - i)
  |> take 4 |> to_list
  |> OUnit.assert_equal [ 100; 99; 98; 97 ]

let test line p = OUnit.assert_bool (spf "test at %d" line) p
let () = test __LINE__ @@ (of_list [ 1; 2; 3; 4 ] |> sorted)
let () = test __LINE__ @@ not (of_list [ 1; 2; 3; 0; 4 ] |> sorted)
let () = test __LINE__ @@ sorted empty

let () =
  [ 1; 2; 3; 3; 2; 2; 3; 4 ] |> of_list |> group_succ_by ?eq:None |> to_list
  |> OUnit.assert_equal [ [ 1 ]; [ 2 ]; [ 3; 3 ]; [ 2; 2 ]; [ 3 ]; [ 4 ] ]

let () =
  [ 1; 2; 3; 3; 2; 2; 3; 4 ] |> of_list
  |> group_by ?eq:None ?hash:None
  |> sort ?cmp:None |> to_list
  |> OUnit.assert_equal [ [ 1 ]; [ 2; 2; 2 ]; [ 3; 3; 3 ]; [ 4 ] ]

let () =
  [ 1; 2; 3; 3; 2; 2; 3; 4 ] |> of_list |> count ?eq:None ?hash:None
  |> sort ?cmp:None |> to_list
  |> OUnit.assert_equal [ 1, 1; 2, 3; 3, 3; 4, 1 ]

let () =
  [ 1; 2; 2; 3; 4; 4; 4; 3; 3 ]
  |> of_list |> uniq ?eq:None |> to_list
  |> OUnit.assert_equal [ 1; 2; 3; 4; 3 ]

let () =
  [ 42; 1; 2; 3; 4; 5; 4; 3; 2; 1 ]
  |> of_list |> sort_uniq ?cmp:None |> to_list
  |> OUnit.assert_equal [ 1; 2; 3; 4; 5; 42 ]

let () =
  let a = 0 -- 2 in
  let b = of_list [ "a"; "b"; "c" ] in
  let s =
    product a b |> map (fun (x, y) -> y, x) |> to_list |> List.sort compare
  in
  OUnit.assert_equal
    [ "a", 0; "a", 1; "a", 2; "b", 0; "b", 1; "b", 2; "c", 0; "c", 1; "c", 2 ]
    s

let () =
  OUnit.assert_equal [ 0, 1; 0, 2; 1, 2 ] (diagonal_l [ 0; 1; 2 ] |> to_list)

let () =
  OUnit.assert_equal
    [ 0, 1; 0, 2; 1, 2 ]
    (of_list [ 0; 1; 2 ] |> diagonal |> to_list)

let () =
  let s1 = 1 -- 3 in
  let s2 = of_list [ "1"; "2" ] in
  let join_row i j =
    if string_of_int i = j then
      Some (string_of_int i ^ " = " ^ j)
    else
      None
  in
  let s = join ~join_row s1 s2 in
  OUnit.assert_equal [ "1 = 1"; "2 = 2" ] (to_list s);
  ()

let () =
  OUnit.assert_equal
    [ 'a', [ "abc"; "attic" ]; 'b', [ "barbary"; "boom"; "bop" ]; 'c', [] ]
    (group_join_by
       (fun s -> s.[0])
       (of_str "abc")
       (of_list [ "abc"; "boom"; "attic"; "deleted"; "barbary"; "bop" ])
    |> map (fun (c, l) -> c, List.sort Stdlib.compare l)
    |> sort |> to_list)

let () =
  let f x =
    if x < 5 then
      Some (string_of_int x, x + 1)
    else
      None
  in
  unfoldr f 0 |> to_list |> OUnit.assert_equal [ "0"; "1"; "2"; "3"; "4" ]

let () =
  1 -- 5 |> scan ( + ) 0 |> to_list
  |> OUnit.assert_equal ~printer:pp_ilist [ 0; 1; 3; 6; 10; 15 ]

let test = OUnit.assert_equal ~printer:Q.Print.int
let () = test 100 (0 -- 100 |> max_exn ?lt:None)
let () = test 0 (0 -- 100 |> min_exn ?lt:None)
let () = OUnit.assert_equal 6 (of_list [ 1; 2; 3 ] |> sum)

let () =
  let seq = of_list [ 10000.0; 3.14159; 2.71828 ] in
  OUnit.assert_equal ~printer:string_of_float 10005.85987 (sumf seq)

let () =
  let l = to_list (take 0 (of_list [ 1 ])) in
  OUnit.assert_equal ~printer:pp_ilist [] l;
  let l = to_list (take 5 (of_list [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ])) in
  OUnit.assert_equal ~printer:pp_ilist [ 1; 2; 3; 4; 5 ] l;
  ()

let () =
  let n =
    of_list [ true; true; false; true ]
    |> fold_while
         (fun acc b ->
           if b then
             acc + 1, `Continue
           else
             acc, `Stop)
         0
  in
  OUnit.assert_equal 2 n;
  ()

let () = 1 -- 5 |> drop 2 |> to_list |> OUnit.assert_equal [ 3; 4; 5 ]
let () = 1 -- 5 |> rev |> to_list |> OUnit.assert_equal [ 5; 4; 3; 2; 1 ]

let () =
  OUnit.assert_bool "true" (for_all (fun x -> x < 10) (1 -- 9));
  OUnit.assert_bool "false" (not (for_all (fun x -> x < 10) (2 -- 11)));
  OUnit.assert_bool "true" (for_all (fun _ -> false) empty);
  OUnit.assert_bool "nested"
    (for_all
       (fun seq -> not (for_all (fun x -> x < 8) seq))
       (1 -- 10 >|= fun x -> x -- 20));
  ()

let () =
  1 -- 100 |> exists (fun x -> x = 59) |> OUnit.assert_bool "exists";
  1 -- 100
  |> exists (fun x -> x < 0)
  |> (fun x -> not x)
  |> OUnit.assert_bool "not exists";
  ()

let () = 1 -- 1000 |> length |> OUnit.assert_equal 1000

let () =
  let h = 1 -- 5 |> zip_i |> to_hashtbl in
  0 -- 4 |> iter (fun i -> OUnit.assert_equal (i + 1) (Hashtbl.find h i));
  OUnit.assert_equal [ 0; 1; 2; 3; 4 ]
    (hashtbl_keys h |> sort ?cmp:None |> to_list);
  ()

let () =
  let b = Buffer.create 4 in
  let upp = function
    | 'a' .. 'z' as c -> Char.chr (Char.code c - Char.code 'a' + Char.code 'A')
    | c -> c
  in
  ("hello world" |> of_str |> rev |> map upp |> fun seq -> to_buffer seq b);
  OUnit.assert_equal "DLROW OLLEH" (Buffer.contents b);
  ()

let () =
  OUnit.assert_equal ~printer:pp_ilist [ 1; 2; 3; 4 ] (to_list (1 -- 4));
  OUnit.assert_equal ~printer:pp_ilist [ 10; 9; 8; 7; 6 ] (to_list (10 --^ 6));
  OUnit.assert_equal ~printer:pp_ilist [] (to_list (10 -- 4));
  OUnit.assert_equal ~printer:pp_ilist [] (to_list (10 --^ 60));
  ()

let test = OUnit.assert_equal ~printer:Q.Print.(list int)
let () = test [ 1; 2; 3; 4 ] (int_range_by ~step:1 1 4 |> to_list)
let () = test [ 4; 3; 2; 1 ] (int_range_by ~step:~-1 4 1 |> to_list)
let () = test [ 6; 4; 2 ] (int_range_by 6 1 ~step:~-2 |> to_list)
let () = test [] (int_range_by ~step:1 4 1 |> to_list)

let () =
  add_qcheck __LINE__
    Q.(pair small_int small_int)
    (fun (i, j) ->
      let i = Stdlib.min i j and j = Stdlib.max i j in
      i -- j |> to_list = (int_range_by ~step:1 i j |> to_list))

let () =
  add_qcheck __LINE__
    Q.(pair small_int small_int)
    (fun (i, j) ->
      let i = Stdlib.min i j and j = Stdlib.max i j in
      i -- j |> to_rev_list = (int_range_by ~step:~-1 j i |> to_list))

let array_for_all f a =
  try
    for i = 0 to Array.length a - 1 do
      if not (f a.(i)) then raise Exit
    done;
    true
  with Exit -> false

let () =
  add_qcheck __LINE__
    Q.(pair (list int) (1 -- 20))
    (fun (l, n) ->
      let seq = of_list l in
      let a = sample n seq in
      array_for_all (fun x -> exists (( = ) x) seq) a
      && Array.length a = Stdlib.min (length seq) n)

(* regression tests *)

let () =
  let s = take 10 (repeat 1) in
  OUnit.assert_bool "not empty" (not (is_empty s));
  ()

let with_tmp_file f =
  let path = Filename.temp_file "test_iter" "data" in
  try
    let x = f path in
    (try Sys.remove path with _ -> ());
    x
  with e ->
    (try Sys.remove path with _ -> ());
    raise e

let () =
  with_tmp_file @@ fun path ->
  Iter.IO.write_lines path Iter.empty;
  let l = Iter.IO.lines_of path |> Iter.to_list in
  OUnit.assert_equal ~printer:Q.Print.(list @@ Printf.sprintf "%S") [] l

let () =
  let errcode = QCheck_base_runner.run_tests ~colors:false !qchecks in
  if errcode <> 0 then exit errcode

(* map_by_2 tests *)
let test = OUnit.assert_equal ~printer:Q.Print.(list int)
let () = test [] (map_by_2 (fun a _ -> a) (of_list []) |> to_list)

(* Test empty iterator *)
let () = test [ 1 ] (map_by_2 (fun _ b -> b) (of_list [ 1 ]) |> to_list)
let () = test [ 3 ] (map_by_2 (fun _ b -> b) (1 -- 3) |> drop 1 |> to_list)
let () = test [ 9 ] (map_by_2 (fun _ b -> b) (1 -- 9) |> drop 4 |> to_list)

(* Odd number of elements should leave the last element in the iterator.
   For an increasing integer range [1,2k] (fun _ b -> b) returns only
   even numbers so this is sufficient to test that this element is left
   in the iterator. *)
let () = test [ 1 ] (map_by_2 (fun a _ -> a) (1 -- 2) |> to_list)
let () = test [ 2 ] (map_by_2 (fun _ b -> b) (1 -- 2) |> to_list)

(* Test two elements *)
let () = test [ 1; 3; 5; 7; 9 ] (map_by_2 (fun a _ -> a) (1 -- 10) |> to_list)
let () = test [ 2; 4; 6; 8; 10 ] (map_by_2 (fun _ b -> b) (1 -- 10) |> to_list)
(* Test more than two elements *)
