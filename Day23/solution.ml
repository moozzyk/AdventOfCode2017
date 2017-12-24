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

type cpu =
  {
    mutable ip        : int;
    registers         : int array;
    mutable num_muls : int;
  }

let get_value cpu reg_or_value =
  if reg_or_value.[0] < 'a' then
    int_of_string reg_or_value
  else
  begin
    cpu.registers.(Char.code reg_or_value.[0] - (Char.code 'a'))
  end

let set_value cpu reg reg_or_value =
  cpu.registers.((Char.code reg.[0]) - Char.code('a')) <- get_value cpu reg_or_value

let rec solve cpu code =
  if cpu.ip < Array.length code then
    let line = code.(cpu.ip) in
    (match line.(0) with
      | "set" ->
        set_value cpu line.(1) line.(2);
        cpu.ip <- (cpu.ip + 1)
      | "sub" ->
        let reg_value = get_value cpu line.(1) in
        let sub_value = get_value cpu line.(2) in
        set_value cpu line.(1) (string_of_int(reg_value - sub_value));
        cpu.ip <- (cpu.ip + 1)
      | "mul" ->
        let reg_value = get_value cpu line.(1) in
        let sub_value = get_value cpu line.(2) in
        set_value cpu line.(1) (string_of_int(reg_value * sub_value));
        cpu.ip <- (cpu.ip + 1);
        cpu.num_muls <- cpu.num_muls + 1
      | "jnz" ->
        let reg_value = get_value cpu line.(1) in
        let offset = if reg_value != 0 then get_value cpu line.(2) else 1 in
        cpu.ip <- (cpu.ip + offset)
      | _ -> raise(Invalid_argument "Unexpected opcode"));
    solve cpu code

let is_prime n =
  let rec is_prime_aux n div =
    if div > int_of_float (sqrt (float_of_int (n))) then
      true
    else
      if n mod div = 0 then false else is_prime_aux n (div + 1)
  in
  is_prime_aux n 2

let rec solve_2 f t s r =
  if f < t then
    let i = if is_prime f then 0 else 1 in
    solve_2 (f + s) t s (r + i)
  else
    r

let () =
  let code = read_lines "input.txt" in
  let code = List.map (fun l -> Array.of_list (split l " ")) code in
  let code = Array.of_list code in
  let cpu = { ip = 0; registers = Array.make 8 0; num_muls = 0 } in
  solve cpu code;
  print_int cpu.num_muls; print_endline "";
  print_int (solve_2 109900 (126900 + 1) 17 0); print_endline ""


