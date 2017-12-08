let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let process line registers =
  let ensure_reg registers reg =
    if not (Hashtbl.mem registers reg) then Hashtbl.add registers reg 0
  in
  let eval_cond registers reg operation value =
    let actual = Hashtbl.find registers reg in
    match operation with
    | "<" -> actual < value
    | "<=" -> actual <= value
    | "==" -> actual = value
    | ">=" -> actual >= value
    | ">" -> actual > value
    | "!=" -> actual != value
    | _ -> raise (Invalid_argument "Invalid copmarison")
  in
  let regex = Str.regexp "\\([a-z]+\\) \\([a-z]+\\) \\(-?[0-9]+\\) if \\([a-z]+\\) \\(.+\\) \\(-?[0-9]+\\)" in
  let _ = Str.search_forward regex line 0 in
  let reg = Str.matched_group 1 line in
  let operation = Str.matched_group 2 line in
  let value = int_of_string (Str.matched_group 3 line) in
  let cond_reg = Str.matched_group 4 line in
  let cond = Str.matched_group 5 line in
  let cond_value = int_of_string (Str.matched_group 6 line) in
  ensure_reg registers reg; ensure_reg registers cond_reg;
  if eval_cond registers cond_reg cond cond_value then
    let current = Hashtbl.find registers reg in
    match operation with
      | "inc" -> Hashtbl.replace registers reg (current + value)
      | "dec" -> Hashtbl.replace registers reg (current - value)
      | _ -> raise (Invalid_argument "Invalid operation")

let () =
  let registers = Hashtbl.create 1000 in
  (* let regex = Str.regexp "\([a-z]+\) \([a-z]+\) (-?[0-9]+) if \([a-z]+\) .+ (-?[0-9]+)" in *)
  let lines = read_lines "input.txt" in
  List.iter (fun line -> process line registers) lines;
  print_int (Hashtbl.fold (fun k v acc -> if v > acc then v else acc) registers min_int);
  print_endline "";
