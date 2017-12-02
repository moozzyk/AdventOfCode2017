let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec solve input idx offset result =
  if idx = List.length input then result
  else if List.nth input idx = List.nth input ((idx + offset) mod (List.length input)) then
    solve input (idx + 1) offset (result + (List.nth input idx))
  else
    solve input (idx + 1) offset result

let () =
  let line = List.hd (read_lines "input.txt") in
  let chars = explode line in
  let ints = List.map (fun i -> (int_of_char i) - 48) chars in
    print_endline (string_of_int (solve ints 0 1 0));
    print_endline (string_of_int (solve ints 0 ((String.length line) / 2) 0));

