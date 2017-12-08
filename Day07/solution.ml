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

let build_weights lines =
  let weights = Hashtbl.create 2000 in
  let add_weight weights line =
    let regex = Str.regexp {|^\([a-z]+\) (\([0-9]+\))|} in
    let _ = Str.search_forward regex line 0 in
    Hashtbl.add weights (Str.matched_group 1 line) (int_of_string (Str.matched_group 2 line))
  in
  List.iter (fun i -> add_weight weights i) lines; weights

(* let build_graph lines =
  let graph = Hashtbl.create 2000 in
  let add_nodes graph line =
    (* let nodes = split line " -> " in
    let node = List.hd (split (List.hd nodes)) in
    let neighbours = split (List.nth nodes 1) ", " in
    Hashtbl.add graph (List.hd nodes) (List.tl nodes) *)
    (* let regex = Str.regexp {|([a-z]|} in
    let _ = Str.search_forward regex line 0 in
    Hashtbl.add weights (Str.matched_group 1 line) "" *)
    print_int 0
  in
  List.iter (fun i -> add_nodes graph i) lines; graph *)


let test line =
  let regex = Str.regexp {|([a-z]|} in
  let _ = Str.search_forward regex line 0 in
  let rec match_list line group =
    try
      let g = Str.matched_group group line in
      match_list line g + 1
    with
      Not_found -> print_string "no more"
  match_list line regex
  (* print_int 42 *)

let () =
  let lines = read_lines "input.txt" in
  let weights = build_weights lines in
  (* let graph = build_graph lines in *)
  Hashtbl.iter (fun i j -> print_endline i) weights
  (* print_int (Hashtbl.length weights); print_int (Hashtbl.length graph); *)

  (* let s = "umnqkb (160) -> nbrvl, bcmbao, vfimqtl" in
  let sp = split s "->" in
  let f = List.hd sp in
  let regex = Str.regexp {|^\([a-z]+\) (\([0-9]+\))|} in
  let _ = Str.search_forward regex f 0 in
  let m = Str.matched_group 1 f in
  print_string (Str.matched_group 1 f); print_string (Str.matched_group 2 f);
  print_string (List.nth sp 1) *)
