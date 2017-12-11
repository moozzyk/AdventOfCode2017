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

let rec find_pos list x y z =
  match list with
  | [] -> (x, y, z)
  | hd::tl ->
      match hd with
      | "n" -> find_pos tl x (y + 1) (z - 1)
      | "s" -> find_pos tl x (y - 1) (z + 1)
      | "nw" -> find_pos tl (x - 1) (y + 1) z
      | "se" -> find_pos tl (x + 1) (y - 1) z
      | "ne" -> find_pos tl (x + 1) y (z - 1)
      | "sw" -> find_pos tl (x - 1) y (z + 1)
      | _ -> raise (Invalid_argument "invalid direction")

let () =
  let line = List.hd (read_lines "input.txt")  in
  let directions = split line "," in
  let (x, y, z) = find_pos directions 0 0 0 in
  let distance = max (max (abs(x)) (abs(y))) (abs(z)) in
  print_int distance
