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

let distance x y z =
  max (max (abs(x)) (abs(y))) (abs(z))

let rec find_pos list x y z max_distance =
  let max_distance = max max_distance (distance x y z) in
  match list with
  | [] -> (x, y, z, max_distance)
  | hd::tl ->
      match hd with
      | "n" -> find_pos tl x (y + 1) (z - 1) max_distance
      | "s" -> find_pos tl x (y - 1) (z + 1) max_distance
      | "nw" -> find_pos tl (x - 1) (y + 1) z max_distance
      | "se" -> find_pos tl (x + 1) (y - 1) z max_distance
      | "ne" -> find_pos tl (x + 1) y (z - 1) max_distance
      | "sw" -> find_pos tl (x - 1) y (z + 1) max_distance
      | _ -> raise (Invalid_argument "invalid direction")


let () =
  let line = List.hd (read_lines "input.txt")  in
  let directions = split line "," in
  let (x, y, z, max_dist) = find_pos directions 0 0 0 0 in
  let dist = distance x y z in
  print_string "distance: "; print_int dist; print_endline "";
  print_string "max distance: "; print_int max_dist; print_endline "";
