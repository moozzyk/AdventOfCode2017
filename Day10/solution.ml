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
      | [] -> (list, (List.nth list hd_idx) * (List.nth list ((hd_idx + 1) mod size)))
      | hd::tl ->
        let (left, right) = rotate list hd [] in
        let left = List.rev left in
        let list = left@right in
        let pivot = (skip + hd) mod size in
        let (left, right) = rotate list pivot [] in
        let hd_idx = if hd_idx <= pivot then hd_idx + (size - pivot) else hd_idx - pivot in
        (* print_string "pivot: "; print_int pivot; print_endline "";
        print_list (right@left);
        print_string "hd_idx: "; print_int hd_idx; print_endline ""; print_endline "---";
        print_string "nth "; print_int (List.nth (right@left) hd_idx); print_endline ""; *)
        get_list (right@left) (skip + 1) tl hd_idx
  in
  let (out_list, result) = get_list list 0 lengths 0 in
  result

let () =
  let line = List.hd (read_lines "input.txt")  in
  let lengths = List.map (fun i -> int_of_string i) (split line ",") in
  let list = range 255 0 [] in
  let result = solve list lengths in
  print_int result; print_endline "";