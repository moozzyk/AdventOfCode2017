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

let print_list l =
  List.iter (fun i -> print_string i; print_string " ") l; print_endline ""

let print_debug line ip registers =
  print_string "ip: "; print_int ip; print_string "; ";
  Hashtbl.iter (fun k (v, s) -> print_string k; print_string ": "; print_int v; print_string ","; print_int s; print_string "; ") registers;
  print_endline "";
  print_endline line

let rec execute_1 registers code ip =
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
    | "snd" -> set_snd_value registers parts.(1); execute_1 registers code (ip + 1)
    | "set" -> set_reg_value registers parts.(1) parts.(2); execute_1 registers code (ip + 1)
    | "add" -> op_reg_value registers parts.(1) parts.(2) (fun a b -> a + b); execute_1 registers code (ip + 1)
    | "mod" -> op_reg_value registers parts.(1) parts.(2) (fun a b -> a mod b); execute_1 registers code (ip + 1)
    | "mul" -> op_reg_value registers parts.(1) parts.(2) (fun a b -> a * b); execute_1 registers code (ip + 1)
    | "rcv" -> let last_snd = rcv registers parts.(1) in
               if last_snd != 0 then
                 last_snd
               else
                 execute_1 registers code (ip + 1)
    | "jgz" -> if get_val registers parts.(1) > 0 then
                execute_1 registers code (ip + (get_val registers parts.(2)))
               else
                execute_1 registers code (ip + 1)
    | _ -> raise(Invalid_argument "unexpected code line")

type cpu =
  {
    id                : int;
    mutable ip        : int;
    registers         : (string, int) Hashtbl.t;
    mutable input     : string list;
    mutable output    : string list;
    mutable num_sends : int;
  }

let print_cpu cpu =
  print_string "cpu: "; print_int cpu.id; print_string ", num_sends: "; print_int cpu.num_sends; print_endline "";
  print_string "registers: ";
  Hashtbl.iter (fun k v -> print_string k; print_string ": "; print_int v; print_string ", ") cpu.registers;
  print_endline "";
  print_string "input: "; print_list cpu.input;
  print_string "output: "; print_list cpu.output

let rec run code cpu1 cpu2 =
  let ensure_register cpu register =
    if Hashtbl.mem cpu.registers register = false then
      Hashtbl.add cpu.registers register 0
  in
  let get_val cpu reg_or_value =
    if reg_or_value.[0] < 'a' then
      reg_or_value
    else
      begin
        ensure_register cpu reg_or_value;
        string_of_int (Hashtbl.find cpu.registers reg_or_value)
      end
  in
  let set_val cpu register value =
    ensure_register cpu register;
    Hashtbl.replace cpu.registers register (int_of_string(get_val cpu value))
  in
  let op_reg_value cpu register arg operation =
    ensure_register cpu register;
    let reg_val = Hashtbl.find cpu.registers register in
    let operand = int_of_string (get_val cpu arg) in
    set_val cpu register (string_of_int(operation reg_val operand))
  in
  let line = code.(cpu1.ip) in
  (* print_cpu cpu1;
  print_endline "";
  print_endline line; *)
  let parts = Array.of_list (split line " ") in
  match parts.(0) with
    | "snd" ->
      let v = get_val cpu1 parts.(1) in
      let l = cpu1.output@[v] in
      cpu1.output <- l;
      cpu2.input <- l;
      cpu1.ip <- cpu1.ip + 1;
      cpu1.num_sends <- cpu1.num_sends + 1;
      run code cpu1 cpu2
    | "rcv" ->
      (match cpu1.input with
      | [] -> ()
      | hd::tl ->
        set_val cpu1 parts.(1) hd;
        cpu1.input <- tl;
        cpu2.output <- tl;
        cpu1.ip <- cpu1.ip + 1;
        run code cpu1 cpu2)
    | "set" -> set_val cpu1 parts.(1) parts.(2); cpu1.ip <- cpu1.ip + 1; run code cpu1 cpu2
    | "add" -> op_reg_value cpu1 parts.(1) parts.(2) (fun a b -> a + b); cpu1.ip <- cpu1.ip + 1; run code cpu1 cpu2
    | "mod" -> op_reg_value cpu1 parts.(1) parts.(2) (fun a b -> a mod b); cpu1.ip <- cpu1.ip + 1; run code cpu1 cpu2
    | "mul" -> op_reg_value cpu1 parts.(1) parts.(2) (fun a b -> a * b); cpu1.ip <- cpu1.ip + 1; run code cpu1 cpu2
    | "jgz" ->
      if int_of_string(get_val cpu1 parts.(1)) > 0 then
        cpu1.ip <- cpu1.ip + (int_of_string(get_val cpu1 parts.(2)))
      else
        cpu1.ip <- cpu1.ip + 1;
      run code cpu1 cpu2
    | _ -> raise(Invalid_argument "unknown instruction")

let rec execute_2 code cpu1 cpu2 =
  run code cpu1 cpu2;
  run code cpu2 cpu1;
  if List.length cpu1.input != 0 || List.length cpu2.input != 0 then execute_2 code cpu1 cpu2

let () =
  let lines = read_lines "input.txt" in
  let code = Array.of_list lines in
  print_int (execute_1 (Hashtbl.create 26) code 0); print_endline "";
  let cpu1 = { id = 1; ip = 0; registers = Hashtbl.create 20; input = []; output = []; num_sends = 0 } in
  let cpu2 = { id = 2; ip = 0; registers = Hashtbl.create 20; input = []; output = []; num_sends = 0 } in
  Hashtbl.add cpu1.registers "p" 0;
  Hashtbl.add cpu1.registers "p" 1;
  execute_2 code cpu1 cpu2;
  print_int cpu1.num_sends; print_endline ""
