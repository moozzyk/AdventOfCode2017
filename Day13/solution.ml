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

let rec get_severity firewall layer severity =
  let get_layer_severity firewall layer =
    let range = Hashtbl.find_opt firewall layer in
    match range with
    | None -> 0
    | Some r -> if layer mod (2 * (r - 1)) = 0 then r * layer else 0
  in
  if layer > 100
    then severity
    else
        let layer_severity = get_layer_severity firewall layer in
        get_severity firewall (layer + 1) (severity + layer_severity)

let () =
  let lines = read_lines "input.txt" in
  let firewall = Hashtbl.create 100 in
  List.iter (fun l -> let s = split l ": " in Hashtbl.add firewall (int_of_string(List.hd s)) (int_of_string(List.nth s 1))) lines;
  print_int_endline (get_severity firewall 0 0)