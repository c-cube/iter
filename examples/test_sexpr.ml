
(** {2 Test iterators} *)

(** print a list of items using the printing function *)
let pp_list ?(sep=", ") pp_item formatter l = 
  Iter.pp_seq ~sep pp_item formatter (Iter.of_list l)

(** Set of integers *)
module ISet = Set.Make(struct type t = int let compare = compare end)
let iset = (module ISet : Set.S with type elt = int and type t = ISet.t)

module OrderedString = struct type t = string let compare = compare end
module SMap = Iter.Map.Make(OrderedString)

let my_map = SMap.of_seq (Iter.of_list ["1", 1; "2", 2; "3", 3; "answer", 42])

let sexpr = "(foo bar (bazz quux hello 42) world (zoo foo bar (1 2 (3 4))))"

type term = | Lambda of term | Const of string | Var of int | Apply of term * term

let random_term () =
  let max = 10
  and num = ref 0 in
  let rec build depth =
    if depth > 4 || !num > max then Const (random_const ()) else
    match Random.int 6 with
    | 0 -> if depth > 0 then Var (Random.int depth) else Const (random_const ())
    | 1 -> incr num; Lambda (build (depth+1))
    | 2 -> Const (random_const ())
    | _ -> incr num; Apply ((build depth), (build depth))
  and random_const () = [|"a"; "b"; "c"; "f"; "g"; "h"|].(Random.int 6)
  in build 0

let rec sexpr_of_term t =
  let f t k = match t with
    | Var i -> Sexpr.output_str "var" (string_of_int i) k
    | Lambda t' -> Sexpr.output_seq "lambda" (sexpr_of_term t') k
    | Apply (t1, t2) -> Sexpr.output_seq "apply" (Iter.append (sexpr_of_term t1) (sexpr_of_term t2)) k
    | Const s -> Sexpr.output_str "const" s k
  in Iter.from_iter (f t)

let term_parser =
  let open Sexpr in
  let rec p_term () =
    left >>
    (("lambda", p_lambda) ^|| ("var", p_var) ^|| ("const", p_const) ^||
      ("apply", p_apply) ^|| fail "bad term") >>= fun x ->
    right >> return x
  and p_apply () =
    p_term () >>= fun x ->
    p_term () >>= fun y ->
    return (Apply (x,y))
  and p_var () = p_int >>= fun i -> return (Var i)
  and p_const () = p_str >>= fun s -> return (Const s)
  and p_lambda () = p_term () >>= fun t -> return (Lambda t)
  in p_term ()

let term_of_sexp seq = Sexpr.parse term_parser seq

let test_term () =
  let t = random_term () in
  Format.printf "@[<h>random term: %a@]@." Sexpr.pp_tokens (sexpr_of_term t);
  let tokens = sexpr_of_term t in
  let t' = term_of_sexp tokens in
  Format.printf "@[<h>parsed: %a@]@." Sexpr.pp_tokens (sexpr_of_term t');
  ()

let _ =
  (* lists *)
  let l = [0;1;2;3;4;5;6] in
  let l' = Iter.to_list
    (Iter.filter (fun x -> x mod 2 = 0) (Iter.of_list l)) in
  let l'' = Iter.to_list
    (Iter.take 3 (Iter.drop 1 (Iter.of_list l))) in
  let h = Hashtbl.create 3 in
  for i = 0 to 5 do
    Hashtbl.add h i (i*i);
  done;
  let l2 = Iter.to_list
    (Iter.map (fun (x, y) -> (string_of_int x) ^ " -> " ^ (string_of_int y))
      (Iter.of_hashtbl h))
  in
  let l3 = Iter.to_list (Iter.rev (Iter.int_range ~start:0 ~stop:42)) in
  let set = List.fold_left (fun set x -> ISet.add x set) ISet.empty [4;3;100;42] in
  let l4 = Iter.to_list (Iter.of_set iset set) in
  Format.printf "l=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l;
  Format.printf "l'=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l';
  Format.printf "l''=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l'';
  Format.printf "l2=@[<h>[%a]@]@." (pp_list Format.pp_print_string) l2;
  Format.printf "l3=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l3;
  Format.printf "s={@[<h>%a@]}@." (Iter.pp_seq Format.pp_print_int) (Iter.of_set iset set);
  Format.printf "l4=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l4;
  Format.printf "l3[:5]+l4=@[<h>[%a]@]@." (Iter.pp_seq Format.pp_print_int)
    (Iter.of_array
      (Iter.to_array (Iter.append
        (Iter.take 5 (Iter.of_list l3)) (Iter.of_list l4))));
  (* iterator, persistent, etc *)
  let seq = Iter.int_range ~start:0 ~stop:100000 in
  let seq' = Iter.persistent seq in
  let stream = Iter.to_stream seq' in
  Format.printf "test length [0..100000]: persistent1 %d, stream %d, persistent2 %d"
    (Iter.length seq') (Iter.length (Iter.of_stream stream)) (Iter.length seq');
  (* maps *)
  Format.printf "@[<h>map: %a@]@."
    (Iter.pp_seq (fun formatter (k,v) -> Format.fprintf formatter "\"%s\" -> %d" k v))
    (SMap.to_seq my_map);
  let module MyMapSeq = Iter.Map.Adapt(Map.Make(OrderedString)) in
  let my_map' = MyMapSeq.of_seq (Iter.of_list ["1", 1; "2", 2; "3", 3; "answer", 42]) in
  Format.printf "@[<h>map: %a@]@."
    (Iter.pp_seq (fun formatter (k,v) -> Format.fprintf formatter "\"%s\" -> %d" k v))
    (MyMapSeq.to_seq my_map');
  (* sum *)
  let n = 1000000 in
  let sum = Iter.fold (+) 0 (Iter.take n (Iter.repeat 1)) in
  Format.printf "%dx1 = %d@." n sum;
  assert (n=sum);
  (* sexpr *)
  let s = Sexpr.of_seq (Sexpr.lex (Iter.of_str sexpr)) in
  let s = Sexpr.of_seq (Iter.map
    (function | `Atom s -> `Atom (String.capitalize_ascii s) | tok -> tok)
    (Sexpr.traverse s))
  in
  Format.printf "@[<hov2>transform @[<h>%s@] into @[<h>%a@]@]@." sexpr (Sexpr.pp_sexpr ~indent:false) s;
  Format.printf "@[<hv2> cycle:%a@]@." Sexpr.pp_tokens
    (Iter.concat (Iter.take 10 (Iter.repeat (Sexpr.traverse s))));
  (* sexpr parsing/printing *)
  for i = 0 to 20 do
    Format.printf "%d-th term test@." i;
    test_term ();
  done;
  ()
