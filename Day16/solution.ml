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

let run arr operations =
  let execute arr operation =
    let swap arr i j =
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp
    in
    let rec reverse arr i j =
      if i < j then
      begin
        swap arr i j;
        reverse arr (i+1) (j-1)
      end
    in
    let rotate arr n =
        reverse arr 0 ((Array.length arr)-1);
        reverse arr 0 (n - 1);
        reverse arr n ((Array.length arr)-1)
    in
    let rec find_idx arr item idx =
      if idx < Array.length arr then
        if arr.(idx) = item then idx else find_idx arr item (idx + 1)
      else
        - 1
    in
    let opcode = operation.[0] in
    let args = String.sub operation 1 ((String.length operation) - 1) in
    match opcode with
    | 's' -> rotate arr (int_of_string(args))
    | 'x' ->
        let parts = split args "/" in
        swap arr (int_of_string(List.hd parts)) (int_of_string(List.hd (List.tl parts)))
    | 'p' ->
        let parts = split args "/" in
        let i = find_idx arr (List.hd parts) 0 in
        let j = find_idx arr (List.hd (List.tl parts)) 0 in
        swap arr i j
    | _ -> raise(Invalid_argument "operation")
  in
  List.iter (fun op -> execute arr op) operations

let solve operations num_iter =
  let join arr = Array.fold_left (fun acc s -> acc ^ s) "" arr in
  let rec aux arr i num_iter cache =
    if i < num_iter then
      let hash = join arr in
      let cycle = Hashtbl.find_opt cache hash in
        match cycle with
        | None ->
          Hashtbl.add cache hash i;
          run arr operations;
          aux arr (i + 1) num_iter cache
        | Some c ->
          let cycle_size = i - c in
          let next_iter = i + (((num_iter - i) / cycle_size) * cycle_size) + 1 in
          run arr operations;
          aux arr next_iter num_iter cache
  in
  let arr = [|"a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p";|] in
  let cache = Hashtbl.create 100000 in
  aux arr 0 num_iter cache;
  print_string (join arr); print_endline ""

let () =
  let line = List.hd (read_lines "input.txt") in
  let operations = split line "," in
  solve operations 1;
  solve operations 1000000000;
