let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let print_state m s e r c =
  for i = s to e do
    for j = s to e do
      let x = (i = r && j = c) in
      if x then print_char '[' else print_char ' ';
      let c = Hashtbl.find_opt m (i, j) in
      match c with
      | None -> print_char '.'
      | Some s -> print_char s;
      if x then print_char ']' else print_char ' ';
    done;
    print_endline ""
  done;
  print_endline ""

let read_infected lines =
  let rec aux lines idx map =
    let rec process_line line row col =
      if col < String.length line then
      begin
        if line.[col] = '#' then Hashtbl.add map (row, col) '#';
        process_line line row (col + 1)
      end
    in
    match lines with
    | [] -> ()
    | hd::tl -> process_line hd idx 0; aux tl (idx + 1) map
  in
  let map = Hashtbl.create 2000 in
  aux lines 0 map;
  map

(*
direction:
 - 0 - up
 - 1 - right
 - 2 - down
 - 3 - left
*)
let solve_1 infected_cells row col direction iterations =
  let rec burst infected_cells row col direction iteration num_infections =
    (* print_state infected_cells (-5) 5 row col; *)
    if iteration > 0 then
    begin
      let cell_infected = Hashtbl.mem infected_cells (row, col) in
      let direction = (if cell_infected then (direction + 1) else (direction + 3)) mod 4 in
      if cell_infected then
      begin
        Hashtbl.remove infected_cells (row, col)
      end
      else
        Hashtbl.add infected_cells (row, col) '#';
      (* cell_infected tells if the cell was originally infected so the value needs to be flipped *)
      let num_infections = num_infections + (if cell_infected then 0 else 1) in
      match direction with
      | 0 -> burst infected_cells (row - 1) col direction (iteration - 1) num_infections
      | 1 -> burst infected_cells row (col + 1) direction (iteration - 1) num_infections
      | 2 -> burst infected_cells (row + 1) col direction (iteration - 1) num_infections
      | 3 -> burst infected_cells row (col - 1) direction (iteration - 1) num_infections
      | _ -> raise(Invalid_argument "Unexpected direction")
    end
    else num_infections
  in
  burst infected_cells row col direction iterations 0

let solve_2 infected_cells row col direction iterations =
  let rec burst infected_cells row col direction iteration num_infections =
    if iteration > 0 then
    begin
      let cell_state = Hashtbl.find_opt infected_cells (row, col) in
      let (direction, infected) =
        match cell_state with
        | None -> Hashtbl.add infected_cells (row, col) 'W'; (direction + 3, false)
        | Some '#' -> Hashtbl.replace infected_cells (row, col) 'F'; (direction + 1, false)
        | Some 'F' -> Hashtbl.remove infected_cells (row, col); (direction + 2, false)
        | Some 'W' -> Hashtbl.replace infected_cells (row, col) '#'; (direction, true)
        | _ -> raise(Invalid_argument "unexpected state") in
      let direction = direction mod 4 in
      let num_infections = num_infections + (if infected then 1 else 0) in
      match direction with
      | 0 -> burst infected_cells (row - 1) col direction (iteration - 1) num_infections
      | 1 -> burst infected_cells row (col + 1) direction (iteration - 1) num_infections
      | 2 -> burst infected_cells (row + 1) col direction (iteration - 1) num_infections
      | 3 -> burst infected_cells row (col - 1) direction (iteration - 1) num_infections
      | _ -> raise(Invalid_argument "Unexpected direction")
    end
    else num_infections
  in
  burst infected_cells row col direction iterations 0

let () =
  let lines = read_lines "input.txt" in
  let infected = read_infected lines in
  let num_infections = solve_1 infected ((List.length lines) / 2) ((String.length (List.hd lines)) / 2) 0 10000 in
  print_int num_infections; print_endline "";
  let num_infections = solve_2 infected ((List.length lines) / 2) ((String.length (List.hd lines)) / 2) 0 10000000 in
  print_int num_infections; print_endline ""
