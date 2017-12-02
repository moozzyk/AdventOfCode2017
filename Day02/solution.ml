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

(* Problem 1 *)
let rec min_max_diff l minimum maximum =
  match l with
  | [] -> maximum - minimum
  | h::t -> min_max_diff t (min h minimum) (max h maximum)

(* Problem 2 *)
let option_get x =
  match x with
  | Some c -> c
  | None -> raise (Invalid_argument "Unexpected None value")

let rec find_even_div list idx =
  if idx < List.length list then
    let dividend = List.nth list idx in
    let divisor = List.find_opt (fun item -> (dividend != item) && (dividend mod item) = 0) list in
    if divisor = None then
      find_even_div list (idx + 1)
    else
      dividend / (option_get divisor)
  else
    raise (Invalid_argument "Unexpected condition")

let solve int_lists f =
  let diffs = List.map (fun item -> f item) int_lists in
  List.fold_left (+) 0 diffs

let () =
  let preprocess string =
    let str_array = split string "\t" in
    List.map (fun item -> int_of_string item) str_array in
  let lines = read_lines "input.txt" in
  let int_lists = List.map (fun item -> preprocess item) lines in
  print_int (solve int_lists (fun item -> min_max_diff item max_int min_int)); print_endline "";
  print_int (solve int_lists (fun item -> find_even_div item 0)); print_endline ""