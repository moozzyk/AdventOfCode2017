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

let rec solve parts input (strength, length) max_b =
  let max_bridge = ref (strength, length) in
  for i = 0 to (Array.length parts) - 1 do
    let (end1, end2) = parts.(i) in
    if end1 = input then
    begin
      parts.(i) <- (-1, -1);
      let (strength, length) = solve parts end2 (strength + end1 + end2, length + 1) max_b in
      max_bridge := max_b !max_bridge (strength, length);
      parts.(i) <- (end1, end2)
    end
    else if end2 = input then
    begin
      parts.(i) <- (-1, -1);
      let (strength, length) = solve parts end1 (strength + end1 + end2, length + 1) max_b in
      max_bridge := max_b !max_bridge (strength, length);
      parts.(i) <- (end1, end2)
    end
  done;
  !max_bridge

let () =
  let get_part l =
    let tmp = split l "/" in
    let end1 = int_of_string(List.hd tmp) in
    let end2 = int_of_string(List.hd (List.tl tmp)) in
    (end1, end2)
  in
  let lines = read_lines "input.txt" in
  let parts = Array.of_list (List.map (fun l -> get_part l) lines) in
  let strongest (max_strength, max_length) (strength, length) =
    if strength > max_strength then (strength, length) else (max_strength, max_length) in
  let longest_strongest (max_strength, max_length) (strength, length) =
    if length > max_length || (length = max_length && strength > max_strength)
      then (strength, length)
      else (max_strength, max_length) in
  let (strength, length) = solve parts 0 (0, 0) strongest in
  print_int strength; print_endline "";
  let (strength, length) = solve parts 0 (0, 0) longest_strongest in
  print_int strength; print_endline "";
