
(** {2 Test sequences} *)

(** print a list of items using the printing function *)
let pp_list ?(sep=", ") pp_item formatter l = 
  Sequence.pp_seq ~sep pp_item formatter (Sequence.List.to_seq l)

(** Set of integers *)
module ISet = Set.Make(struct type t = int let compare = compare end)

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
  let s = (module ISet : Set.S with type elt = int and type t = ISet.t) in
  let l4 = Sequence.List.of_seq (Sequence.Set.to_seq s set) in
  Format.printf "l=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l;
  Format.printf "l'=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l';
  Format.printf "l''=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l'';
  Format.printf "l2=@[<h>[%a]@]@." (pp_list Format.pp_print_string) l2;
  Format.printf "l3=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l3;
  Format.printf "l4=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l4;
  ()
