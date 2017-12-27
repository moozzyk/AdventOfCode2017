let set_value tape cursor value =
  Hashtbl.remove tape cursor;
  Hashtbl.add tape cursor value

let get_value tape cursor =
  let v = Hashtbl.find_opt tape cursor in
  match v with
  | Some value -> value
  | None -> 0

type action =
  {
    value : int;
    cursor_offset: int;
    state : string;
  }

let rec solve rules num_steps state tape cursor =
  let execute_action rules action num_steps tape cursor =
    set_value tape cursor (action.value);
    solve rules (num_steps - 1) action.state tape (cursor + action.cursor_offset)
  in
  if num_steps = 0 then
    Hashtbl.fold (fun k v acc -> acc + v) tape 0
  else
    let (action_0, action_1) = Hashtbl.find rules state in
    let current_value = get_value tape cursor in
    execute_action rules (if current_value = 0 then action_0 else action_1) num_steps tape cursor

let () =
  let tape = Hashtbl.create 1000 in
  let rules = Hashtbl.create 10 in
  Hashtbl.add rules "A" ({value = 1; cursor_offset = 1; state = "B"}, {value = 0; cursor_offset = (-1); state = "F"});
  Hashtbl.add rules "B" ({value = 0; cursor_offset = 1; state = "C"}, {value = 0; cursor_offset = 1; state = "D"});
  Hashtbl.add rules "C" ({value = 1; cursor_offset = (-1); state = "D"}, {value = 1; cursor_offset = 1; state = "E"});
  Hashtbl.add rules "D" ({value = 0; cursor_offset = (-1); state = "E"}, {value = 0; cursor_offset = (-1); state = "D"});
  Hashtbl.add rules "E" ({value = 0; cursor_offset = 1; state = "A"}, {value = 1; cursor_offset = 1; state = "C"});
  Hashtbl.add rules "F" ({value = 1; cursor_offset = (-1); state = "A"}, {value = 1; cursor_offset = 1; state = "A"});
  print_int(solve rules 12994925 "A" tape 0); print_endline ""
