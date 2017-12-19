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

let print_debug line ip registers =
  print_string "ip: "; print_int ip; print_string "; ";
  Hashtbl.iter (fun k (v, s) -> print_string k; print_string ": "; print_int v; print_string ","; print_int s; print_string "; ") registers;
  print_endline "";
  print_endline line

let rec execute registers code ip =
  let ensure_register registers register =
    if Hashtbl.mem registers register = false then
      Hashtbl.add registers register (0, 0)
  in
  let get_val registers reg_or_value =
    if reg_or_value.[0] < 'a' then
      int_of_string reg_or_value
    else
      begin
        ensure_register registers reg_or_value;
        let (reg_val, _) = Hashtbl.find registers reg_or_value in
        reg_val
      end
  in
  let set_snd_value registers register =
    ensure_register registers register;
    let (reg_val, _) = Hashtbl.find registers register in
    Hashtbl.replace registers register (reg_val, reg_val)
  in
  let set_reg_value registers register arg =
    ensure_register registers register;
    let (reg_val, snd) = Hashtbl.find registers register in
    let new_val = get_val registers arg in
    Hashtbl.replace registers register (new_val, snd)
  in
  let op_reg_value registers register arg operation =
    ensure_register registers register;
    let (reg_val, _) = Hashtbl.find registers register in
    let operand = get_val registers arg in
    set_reg_value registers register (string_of_int(operation reg_val operand))
  in
  let rcv registers register =
    let (_, snd) = Hashtbl.find registers register in snd
  in
  let line = code.(ip) in
  (* print_debug line ip registers; *)
  let parts = Array.of_list (split line " ") in
  match parts.(0) with
    | "snd" -> set_snd_value registers parts.(1); execute registers code (ip + 1)
    | "set" -> set_reg_value registers parts.(1) parts.(2); execute registers code (ip + 1)
    | "add" -> op_reg_value registers parts.(1) parts.(2) (fun a b -> a + b); execute registers code (ip + 1)
    | "mod" -> op_reg_value registers parts.(1) parts.(2) (fun a b -> a mod b); execute registers code (ip + 1)
    | "mul" -> op_reg_value registers parts.(1) parts.(2) (fun a b -> a * b); execute registers code (ip + 1)
    | "rcv" -> let last_snd = rcv registers parts.(1) in
               if last_snd != 0 then
                 last_snd
               else
                 execute registers code (ip + 1)
    | "jgz" -> if get_val registers parts.(1) > 0 then
                execute registers code (ip + (get_val registers parts.(2)))
               else
                execute registers code (ip + 1)
    | _ -> raise(Invalid_argument "unexpected code line")

let () =
  let lines = read_lines "input.txt" in
  let code = Array.of_list lines in
  print_int (execute (Hashtbl.create 26) code 0); print_endline ""