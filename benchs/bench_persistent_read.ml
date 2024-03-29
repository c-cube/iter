module MList = struct
  type 'a t = {
    content: 'a array; (* elements of the node *)
    mutable len: int; (* number of elements in content *)
    mutable tl: 'a t; (* tail *)
  }
  (** A list that contains some elements, and may point to another list *)

  (** Empty list, for the tl field *)
  let _empty () : 'a t = Obj.magic 0

  let make n =
    assert (n > 0);
    { content = Array.make n (Obj.magic 0); len = 0; tl = _empty () }

  let rec is_empty l = l.len = 0 && (l.tl == _empty () || is_empty l.tl)

  let rec iter f l =
    for i = 0 to l.len - 1 do
      f l.content.(i)
    done;
    if l.tl != _empty () then iter f l.tl

  let iteri f l =
    let rec iteri i f l =
      for j = 0 to l.len - 1 do
        f (i + j) l.content.(j)
      done;
      if l.tl != _empty () then iteri (i + l.len) f l.tl
    in
    iteri 0 f l

  let rec iter_rev f l =
    if l.tl != _empty () then iter_rev f l.tl;
    for i = l.len - 1 downto 0 do
      f l.content.(i)
    done

  let length l =
    let rec len acc l =
      if l.tl == _empty () then
        acc + l.len
      else
        len (acc + l.len) l.tl
    in
    len 0 l

  (** Get element by index *)
  let rec get l i =
    if i < l.len then
      l.content.(i)
    else if i >= l.len && l.tl == _empty () then
      raise (Invalid_argument "MList.get")
    else
      get l.tl (i - l.len)

  (** Push [x] at the end of the list. It returns the block in which the
      element is inserted. *)
  let rec push x l =
    if l.len = Array.length l.content then (
      (* insert in the next block *)
      if l.tl == _empty () then (
        let n = Array.length l.content in
        l.tl <- make (n + (n lsr 1))
      );
      push x l.tl
    ) else (
      (* insert in l *)
      l.content.(l.len) <- x;
      l.len <- l.len + 1;
      l
    )

  (** Reverse list (in place), and returns the new head *)
  let rev l =
    let rec rev prev l =
      (* reverse array *)
      for i = 0 to (l.len - 1) / 2 do
        let x = l.content.(i) in
        l.content.(i) <- l.content.(l.len - i - 1);
        l.content.(l.len - i - 1) <- x
      done;
      (* reverse next block *)
      let l' = l.tl in
      l.tl <- prev;
      if l' == _empty () then
        l
      else
        rev l l'
    in
    rev (_empty ()) l

  (** Build a MList of elements of the Seq. The optional argument indicates
      the size of the blocks *)
  let of_seq ?(size = 8) seq =
    (* read iterator into a MList.t *)
    let start = make size in
    let l = ref start in
    seq (fun x -> l := push x !l);
    start

  let to_seq l k = iter k l
end

(** Store content of the seqerator in an enum *)
let persistent_mlist seq =
  let l = MList.of_seq seq in
  MList.to_seq l

let bench_mlist n = persistent_mlist Iter.(1 -- n)

let bench_list n =
  let l = Iter.to_rev_list Iter.(1 -- n) in
  Iter.of_list (List.rev l)

let bench_naive n =
  let s = Iter.(1 -- n) in
  Iter.iter ignore s;
  s

let bench_current n = Iter.persistent Iter.(1 -- n)

let bench_array n =
  let a = Iter.to_array Iter.(1 -- n) in
  Iter.of_array a

let read s = Iter.map (fun x -> x + 1) s

let () =
  let bench_n n =
    Printf.printf "BENCH for %d\n" n;
    let res =
      let mlist = bench_mlist n in
      let list = bench_list n in
      let current = bench_current n in
      let array = bench_current n in
      let naive = bench_naive n in
      Benchmark.throughputN 5
        [
          "mlist", read, mlist;
          "list", read, list;
          "current", read, current;
          "array", read, array;
          "naive", read, naive;
        ]
    in
    Benchmark.tabulate res
  in
  bench_n 100;
  bench_n 100_000;
  ()

(* vim:Use benchmark: *)
