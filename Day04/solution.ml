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

let is_valid l =
  let rec has_dups words dict =
    match words with
    | [] -> 1
    | h::t -> if Hashtbl.mem dict h then 0 else begin Hashtbl.add dict h "";has_dups t dict end
  in
  let words = split l " " in
  let dict = Hashtbl.create 100 in
  has_dups words dict

let () =
  let lines = read_lines "input.txt" in
  print_int (List.fold_left (fun a i -> a + (is_valid i)) 0 lines)