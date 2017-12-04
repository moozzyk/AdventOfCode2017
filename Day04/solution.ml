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

let rec has_dups words dict =
  match words with
  | [] -> 1
  | h::t -> if Hashtbl.mem dict h then 0 else begin Hashtbl.add dict h "";has_dups t dict end

let rec has_dup_anagrams words dict =
  match words with
  | [] -> 1
  | h::t ->
    let w = explode h in
    let sorted_w = List.sort (fun l r -> if l < r then 1 else if l = r then 0 else - 1) w in
    if Hashtbl.mem dict sorted_w then 0 else begin Hashtbl.add dict sorted_w "";has_dup_anagrams t dict end


let () =
  let lines = read_lines "input.txt" in
  let is_valid l f =
    let words = split l " " in
    let dict = Hashtbl.create 100 in
    f words dict
  in
  print_int (List.fold_left (fun a i -> a + (is_valid i has_dups)) 0 lines); print_endline "";
  print_int (List.fold_left (fun a i -> a + (is_valid i has_dup_anagrams)) 0 lines); print_endline "";