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

let rec solve char tail garbage indent score =
  if tail = [] then
    (* tail is empty but need to add one since char should be the last '}' *)
    1 + score
  else if char = '!' then
    solve (List.hd (List.tl tail)) (List.tl (List.tl tail)) garbage indent score
  else if garbage then
    if char = '>' then
      solve (List.hd tail) (List.tl tail) false indent score
    else
      solve (List.hd tail) (List.tl tail) garbage indent score
  else
    match char with
    | '<' -> solve (List.hd tail) (List.tl tail) true indent score
    | '{' -> solve (List.hd tail) (List.tl tail) false (indent + 1) score
    | '}' -> solve (List.hd tail) (List.tl tail) false (indent - 1) (score + indent)
    | _ -> solve (List.hd tail) (List.tl tail) false indent score

let rec solve_2 char tail garbage count =
  if tail = [] then
    (* the assumption is that the string does not end with '>' *)
    count
  else if char = '!' then
    solve_2 (List.hd (List.tl tail)) (List.tl (List.tl tail)) garbage count
  else if garbage then
    if char = '>' then
      solve_2 (List.hd tail) (List.tl tail) false count
    else
      solve_2 (List.hd tail) (List.tl tail) garbage (count + 1)
  else if char = '<' then
    solve_2 (List.hd tail) (List.tl tail) true count
  else
    solve_2 (List.hd tail) (List.tl tail) false count

let () =
  let line = List.hd (read_lines "input.txt") in
  let chars = explode line in
  print_int (solve (List.hd chars) (List.tl chars) false 0 0); print_endline "";
  print_int (solve_2 (List.hd chars) (List.tl chars) false 0); print_endline ""