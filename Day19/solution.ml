let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

(*
direction:
- 0 - up
- 1 - right
- 2 - down
- 3 - left
*)
let rec solve network row col direction result num_steps =
  let is_letter char = char >= 'A' && char <= 'Z' in
  let tile = network.(row).[col] in
  (* print_int row; print_string " "; print_int col; print_string " "; print_int direction; print_string " ";
  print_char tile; print_endline ""; *)
  if tile = ' ' then
    (result, num_steps)
  else if tile = '+' then
  begin
    if direction != 2 && row > 0 && (network.(row - 1).[col] = '|' || is_letter network.(row - 1).[col]) then
      solve network (row - 1) col (*up*) 0 result (num_steps + 1)
    else if direction != 0 && row < ((Array.length network) - 1) && (network.(row + 1).[col] = '|' || is_letter network.(row + 1).[col]) then
      solve network (row + 1) col (*down*) 2 result (num_steps + 1)
    else if direction != 3 && col < ((String.length network.(row)) - 1) && (network.(row).[col + 1] = '-' || is_letter network.(row).[col + 1]) then
      solve network row (col + 1) (*right*) 1 result (num_steps + 1)
    else if direction != 1 && col > 0 && (network.(row).[col - 1] = '-' || is_letter network.(row).[col - 1]) then
      solve network row (col - 1) (*left*) 3 result (num_steps + 1)
    else
      raise(Invalid_argument "Unexpected condition")
  end
  else
    let result = if is_letter tile then result ^ String.make 1 tile else result in
    match direction with
      | 0 -> solve network (row - 1) col direction result (num_steps + 1)
      | 1 -> solve network row (col + 1) direction result (num_steps + 1)
      | 2 -> solve network (row + 1) col direction result (num_steps + 1)
      | 3 -> solve network row (col - 1) direction result (num_steps + 1)
      | _ -> raise (Invalid_argument "Unexpected direction")

let () =
  let lines = read_lines "input.txt" in
  let start = (String.length (List.hd lines)) - 1 in
  let longest = List.fold_left (fun acc l -> max (String.length l) acc) 0 lines in
  let lines = List.map (fun l -> l ^ (String.make (longest + 1 - (String.length l)) ' ')) lines in
  let network = Array.of_list lines in
  let (result, steps) = solve network 0 start 2 "" 0 in
  print_string result; print_endline ""; print_int steps; print_endline ""
