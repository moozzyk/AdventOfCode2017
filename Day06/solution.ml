let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let list_to_arr list =
  let rec fill source target idx =
    match source with
    | [] -> ()
    | h::t -> target.(idx) <- h; fill t target (idx + 1)
  in
  let arr = Array.make (List.length list) 0 in
  fill list arr 0; arr

let split string separator =
  Str.split (Str.regexp separator) string

let rec print_arr arr idx =
  if idx < (Array.length arr) then
    begin
      print_int (arr.(idx)); print_string " ";
      print_arr arr (idx + 1)
    end

let solve arr =
  let rec max_bank arr idx max_idx =
    if idx = Array.length arr then
      max_idx
    else
      if arr.(idx) > arr.(max_idx) then
        max_bank arr (idx + 1) idx
      else
        max_bank arr (idx + 1) max_idx
  in
  let rebalance arr idx =
    let count = ref arr.(idx) in
    let i = ref idx in
    arr.(!i) <- 0;
    incr i;
    i := !i mod (Array.length arr);
    while !count > 0 do
      arr.(!i) <- arr.(!i) + 1;
      decr count;
      incr i;
      i := !i mod (Array.length arr)
    done;
  in
  let rec create_hash arr idx hash =
    if idx < (Array.length arr) then
      create_hash arr (idx + 1) (hash ^ (string_of_int arr.(idx)) ^ "|")
    else
      hash
  in
  let rec solve_aux array dict steps =
    let num_bank = max_bank arr 1 0 in
    rebalance array num_bank;
    let hash = create_hash array 0 "" in
    if Hashtbl.mem dict hash then
      steps
    else
      begin
        Hashtbl.add dict hash "";
        solve_aux array dict (steps + 1)
      end
  in
  let dict = Hashtbl.create 1000 in
  solve_aux arr dict 1

let() =
  let line = List.hd (read_lines "input.txt") in
  let arr = list_to_arr (List.map (fun i -> int_of_string i) (split line "\t")) in
  print_int (solve arr); print_endline "";
  print_int ((solve arr) - 1); print_endline "";
