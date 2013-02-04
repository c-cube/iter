
(** {2 Test sequences} *)

(** print a list of items using the printing function *)
let pp_list ?(sep=", ") pp_item formatter l = 
  Sequence.pp_seq ~sep pp_item formatter (Sequence.List.to_seq l)

(** Set of integers *)
module ISet = Set.Make(struct type t = int let compare = compare end)
module ISetSeq = Sequence.Set(ISet)

let sexpr = "(foo bar (bazz quux hello 42) world (zoo foo bar (1 2 (3 4))))"

let _ =
  let l = [0;1;2;3;4;5;6] in
  let l' = Sequence.List.of_seq
    (Sequence.filter (fun x -> x mod 2 = 0) (Sequence.List.to_seq l)) in
  let l'' = Sequence.List.of_seq
    (Sequence.take 3 (Sequence.drop 1 (Sequence.List.to_seq l))) in
  let h = Hashtbl.create 3 in
  for i = 0 to 5 do
    Hashtbl.add h i (i*i);
  done;
  let l2 = Sequence.List.of_seq
    (Sequence.map (fun (x, y) -> (string_of_int x) ^ " -> " ^ (string_of_int y))
      (Sequence.Hashtbl.to_seq h))
  in
  let l3 = Sequence.List.of_seq (Sequence.rev (Sequence.Int.range ~start:0 ~stop:42)) in
  let set = List.fold_left (fun set x -> ISet.add x set) ISet.empty [4;3;100;42] in
  let l4 = Sequence.List.of_seq (ISetSeq.to_seq set) in
  Format.printf "l=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l;
  Format.printf "l'=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l';
  Format.printf "l''=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l'';
  Format.printf "l2=@[<h>[%a]@]@." (pp_list Format.pp_print_string) l2;
  Format.printf "l3=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l3;
  Format.printf "s={@[<h>%a@]}@." (Sequence.pp_seq Format.pp_print_int) (ISetSeq.to_seq set);
  Format.printf "l4=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l4;
  Format.printf "l3[:5]+l4=@[<h>[%a]@]@." (Sequence.pp_seq Format.pp_print_int)
    (Sequence.Array.to_seq
      (Sequence.Array.of_seq (Sequence.append
        (Sequence.take 5 (Sequence.List.to_seq l3)) (Sequence.List.to_seq l4))));
  (* sum *)
  let n = 100000000 in
  let sum = Sequence.fold (+) 0 (Sequence.take n (Sequence.Int.repeat 1)) in
  Format.printf "%dx1 = %d@." n sum;
  assert (n=sum);
  let s = Sexpr.of_seq (Sexpr.lex (Sequence.String.to_seq sexpr)) in
  let s = Sexpr.of_seq (Sequence.map
    (function | `Atom s -> `Atom (String.capitalize s) | tok -> tok)
    (Sexpr.traverse s))
  in
  Format.printf "@[<hov2>transform @[<h>%s@] into @[<h>%a@]@]@." sexpr (Sexpr.pp_sexpr ~indent:false) s;
  ()
