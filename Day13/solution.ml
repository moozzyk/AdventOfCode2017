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

let print_int_endline sev =
  print_int sev; print_endline ""

let rec get_collisions firewall layer delay collisions =
  let is_clean firewall layer delay =
    let range = Hashtbl.find_opt firewall layer in
    match range with
    | None -> true
    | Some r -> (layer + delay) mod (2 * (r - 1)) != 0
  in
  if layer > 100 then
    collisions
  else
    if is_clean firewall layer delay then
      get_collisions firewall (layer + 1) delay collisions
    else
      get_collisions firewall (layer + 1) delay (collisions@[layer])

let rec find_delay firewall delay =
  let collisions = get_collisions firewall 0 delay [] in
  match collisions with
  | [] -> delay
  | _::_ -> find_delay firewall (delay + 1)

let () =
  let lines = read_lines "input.txt" in
  let firewall = Hashtbl.create 100 in
  List.iter (fun l -> let s = split l ": " in Hashtbl.add firewall (int_of_string(List.hd s)) (int_of_string(List.nth s 1))) lines;
  let collisions = get_collisions firewall 0 0 [] in
  let severity = List.fold_left (fun acc layer -> acc + ((Hashtbl.find firewall layer) * layer)) 0 collisions in
  print_int_endline severity;
  print_int_endline (find_delay firewall 0)