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

let solve operations =
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
  let p = [|"a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p";|] in
  List.iter (fun op -> execute p op) operations;
  Array.iter (fun i -> print_string i) p;
  print_endline ""

let () =
  let line = List.hd (read_lines "input.txt") in
  let operations = split line "," in
  solve operations