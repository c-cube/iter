
(** {2 Test sequences} *)

(** print a list of items using the printing function *)
let rec pp_list ?(sep=", ") pp_item formatter = function
  | x::y::xs -> Format.fprintf formatter "%a%s@,%a"
      pp_item x sep (pp_list ~sep:sep pp_item) (y::xs)
  | x::[] -> pp_item formatter x
  | [] -> ()

let _ =
  let l = [0;1;2;3;4;5;6] in
  let l' = Sequence.List.of_seq
    (Sequence.filter (fun x -> x mod 2 = 0) (Sequence.List.to_seq l)) in
  let l'' = Sequence.List.of_seq
    (Sequence.take 3 (Sequence.drop 1 (Sequence.List.to_seq l))) in
  Format.printf "l=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l;
  Format.printf "l'=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l';
  Format.printf "l''=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l'';
