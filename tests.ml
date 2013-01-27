
(** {2 Test sequences} *)

let seq_of_list l = Sequence.from_iter (fun k -> List.iter k l)

let list_of_seq seq = List.rev (Sequence.fold (fun y x -> x::y) [] seq)

let seq_of_hashtbl h =
  Sequence.from_iter (fun k -> Hashtbl.iter (fun a b -> k (a, b)) h)

let hashtbl_from_seq seq =
  let h = Hashtbl.create 3 in
  Sequence.iter (fun (k,v) -> Hashtbl.replace h k v) seq;
  h

(** print a list of items using the printing function *)
let rec pp_list ?(sep=", ") pp_item formatter = function
  | x::y::xs -> Format.fprintf formatter "%a%s@,%a"
      pp_item x sep (pp_list ~sep:sep pp_item) (y::xs)
  | x::[] -> pp_item formatter x
  | [] -> ()

let _ =
  let l = [0;1;2;3;4;5;6] in
  let l' = list_of_seq (Sequence.filter (fun x -> x mod 2 = 0) (seq_of_list l)) in
  let l'' = list_of_seq (Sequence.take 3 (Sequence.drop 1 (seq_of_list l))) in
  Format.printf "l=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l;
  Format.printf "l'=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l';
  Format.printf "l''=@[<h>[%a]@]@." (pp_list Format.pp_print_int) l'';
