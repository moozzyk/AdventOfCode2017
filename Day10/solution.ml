let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let split string separator =
  Str.split (Str.regexp separator) string

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec print_list l =
  match l with
  | [] -> print_endline ""
  | hd::tl -> print_int hd; print_string " "; print_list tl

let rec range mx curr l =
  if curr > mx then l else curr::range mx (curr + 1) l

let rec rotate list idx out_list =
  if idx > 0 then
    let hd = List.hd list in
    rotate (List.tl list) (idx - 1) (out_list@[hd])
  else
    (out_list, list)

let solve list lengths =
  let size = List.length list in
  let rec get_list list skip lengths hd_idx =
    match lengths with
      | [] -> let (left, right) = rotate list hd_idx [] in
        (right@left)
      | hd::tl ->
        let (left, right) = rotate list hd [] in
        let left = List.rev left in
        let list = left@right in
        let pivot = (skip + hd) mod size in
        let (left, right) = rotate list pivot [] in
        let hd_idx = if hd_idx <= pivot then hd_idx + (size - pivot) else hd_idx - pivot in
        get_list (right@left) (skip + 1) tl hd_idx
  in
  get_list list 0 lengths 0

let problem_1 line list =
  let lengths = List.map (fun i -> int_of_string i) (split line ",") in
  let result = solve list lengths in
  print_int (List.hd result * (List.hd (List.tl result))); print_endline ""

(* Problem 2 *)
let problem_2 line list =
  let chars = explode line in
  let lengths = List.map (fun c -> int_of_char c) chars in
  let lengths = (lengths@[17; 31; 73; 47; 23]) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let out_list = solve list lengths in
  let rec fold_left list (idx, acc, res) =
    match list with
    | [] -> res
    | hd::tl ->
      if ((idx + 1) mod 16) = 0 then
        fold_left tl (idx + 1, 0, res@[(acc lxor hd)])
      else
        fold_left tl (idx + 1, acc lxor hd, res)
  in
  let compressed = fold_left out_list (0, 0, []) in
  List.iter (fun i -> Printf.printf "%02x" i) compressed; print_endline ""

let () =
  let line = List.hd (read_lines "input.txt")  in
  problem_1 line (range 255 0 []);
  problem_2 line (range 255 0 [])