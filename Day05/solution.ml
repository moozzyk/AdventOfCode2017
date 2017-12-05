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

let follow_jumps arr =
  let steps = ref 0 in
  let idx = ref 0 in
  while !idx < (Array.length arr) do
    let new_idx = !idx + arr.(!idx) in
    arr.(!idx) <- arr.(!idx) + 1;
    idx := new_idx;
    incr steps;
  done;
  !steps

let () =
  let lines = read_lines "input.txt" in
  let arr = list_to_arr (List.map (fun i -> int_of_string i) lines) in
  print_int (follow_jumps arr)