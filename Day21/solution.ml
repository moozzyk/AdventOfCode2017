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

let rotate_2 s =
  [
    Printf.sprintf "%c%c/%c%c" s.[0] s.[1] s.[3] s.[4];

    (* flip *)
    Printf.sprintf "%c%c/%c%c" s.[3] s.[4] s.[0] s.[1];
    Printf.sprintf "%c%c/%c%c" s.[1] s.[0] s.[4] s.[3];

    (* rotate *)
    Printf.sprintf "%c%c/%c%c" s.[3] s.[0] s.[4] s.[1];
    Printf.sprintf "%c%c/%c%c" s.[4] s.[3] s.[1] s.[0];
    Printf.sprintf "%c%c/%c%c" s.[1] s.[4] s.[0] s.[3];
  ]

let rotate_3 s =
  [
    Printf.sprintf "%c%c%c/%c%c%c/%c%c%c" s.[0] s.[1] s.[2] s.[4] s.[5] s.[6] s.[8] s.[9] s.[10];
    Printf.sprintf "%c%c%c/%c%c%c/%c%c%c" s.[8] s.[9] s.[10] s.[4] s.[5] s.[6] s.[0] s.[1] s.[2];
    Printf.sprintf "%c%c%c/%c%c%c/%c%c%c" s.[8] s.[4] s.[0] s.[9] s.[5] s.[1] s.[10] s.[6] s.[2];
    Printf.sprintf "%c%c%c/%c%c%c/%c%c%c" s.[10] s.[6] s.[2] s.[9] s.[5] s.[1] s.[8] s.[4] s.[0];

    Printf.sprintf "%c%c%c/%c%c%c/%c%c%c" s.[10] s.[9] s.[8] s.[6] s.[5] s.[4] s.[2] s.[1] s.[0];
    Printf.sprintf "%c%c%c/%c%c%c/%c%c%c" s.[2] s.[1] s.[0] s.[6] s.[5] s.[4] s.[10] s.[9] s.[8];

    Printf.sprintf "%c%c%c/%c%c%c/%c%c%c" s.[2] s.[6] s.[10] s.[1] s.[5] s.[9] s.[0] s.[4] s.[8];
    Printf.sprintf "%c%c%c/%c%c%c/%c%c%c" s.[0] s.[4] s.[8] s.[1] s.[5] s.[9] s.[2] s.[6] s.[10];
  ]

let rec create_rulebook rules lines =
  let add_rule rules line =
    let is_short = (line.[6] = '=') in
    let left = if is_short then String.sub line 0 5 else String.sub line 0 11 in
    let right = if is_short then String.sub line 9 11 else String.sub line 15 19 in
    Hashtbl.add rules left right
  in
  match lines with
  | [] -> ()
  | hd::tl -> add_rule rules hd; create_rulebook rules tl

let print_picture picture =
  Array.iter (fun i -> print_string i; print_string " "; print_int ((List.length (String.split_on_char '#' i)) - 1); print_endline "" ) picture;
  print_int (Array.fold_left (fun acc i -> acc + ((List.length (String.split_on_char '#' i)) - 1)) 0 picture); print_endline ""; print_endline ""

let rec get_square rules inputs =
  match inputs with
    | [] -> raise (Invalid_argument "Transform not found")
    | hd::tl ->
      (match Hashtbl.find_opt rules hd with
       | Some v -> v
       | None -> get_square rules tl)

let process_2 rules picture =
  (* print_picture picture; *)
  let size = Array.length picture in
  let result = Array.make (3 * size / 2) "" in
  for i = 0 to (size / 2) - 1 do
    for j = 0 to (size / 2) - 1 do
      let square_pattern = Printf.sprintf "%s/%s"
        (String.sub (picture.(i * 2 + 0)) (j * 2) 2)
        (String.sub (picture.(i * 2 + 1)) (j * 2) 2) in
      let inputs = rotate_2 square_pattern in
      let generated = get_square rules inputs in
        result.(i * 3 + 0) <- result.(i * 3 + 0)^(String.sub generated 0 3);
        result.(i * 3 + 1) <- result.(i * 3 + 1)^(String.sub generated 4 3);
        result.(i * 3 + 2) <- result.(i * 3 + 2)^(String.sub generated 8 3)
    done
  done;
  result

let process_3 rules picture =
  (* print_picture picture; *)
  let size = Array.length picture in
  let result = Array.make (4 * size / 3) "" in
  for i = 0 to (size / 3) - 1 do
    for j = 0 to (size / 3) - 1 do
      let square_pattern = Printf.sprintf "%s/%s/%s"
        (String.sub (picture.(i * 3 + 0)) (j * 3) 3)
        (String.sub (picture.(i * 3 + 1)) (j * 3) 3)
        (String.sub (picture.(i * 3 + 2)) (j * 3) 3) in
      let inputs = rotate_3 square_pattern in
      let generated = get_square rules inputs in
        result.(i * 4 + 0) <- result.(i * 4 + 0)^String.sub generated 0 4;
        result.(i * 4 + 1) <- result.(i * 4 + 1)^String.sub generated 5 4;
        result.(i * 4 + 2) <- result.(i * 4 + 2)^String.sub generated 10 4;
        result.(i * 4 + 3) <- result.(i * 4 + 3)^String.sub generated 15 4
    done
  done;
  result

let rec solve rules picture iteration =
  if iteration = 0 then
    Array.fold_left (fun acc i -> acc + ((List.length (String.split_on_char '#' i)) - 1)) 0 picture
  else
    let size = Array.length picture in
    let picture =
      if size mod 2 = 0 then
        process_2 rules picture
      else
        process_3 rules picture
    in
    solve rules picture (iteration - 1)

let () =
  let lines = read_lines "input.txt" in
  let rules = Hashtbl.create 2000 in
  create_rulebook rules lines;
  let input = [|".#.";"..#";"###"|] in
  let res = solve rules input 5 in
  print_int res; print_endline "";
  let res = solve rules input 18 in
  print_int res; print_endline ""
