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

let rec print_list l =
  match l with
  | [] -> print_endline ""
  | hd::tl -> print_string hd; print_string " "; print_list tl

let build_weights lines =
  let weights = Hashtbl.create 2000 in
  let add_weight weights line =
    let regex = Str.regexp {|^\([a-z]+\) (\([0-9]+\))|} in
    let _ = Str.search_forward regex line 0 in
    Hashtbl.add weights (Str.matched_group 1 line) (int_of_string (Str.matched_group 2 line))
  in
  List.iter (fun i -> add_weight weights i) lines; weights

let build_graph lines =
  let get_nodes line =
    let parts = split line " -> " in
    let left = split (List.hd parts) " " in
    let neighbours = if List.length parts > 1 then split (List.hd (List.tl parts)) ", " else [] in
    (List.hd left)::neighbours
  in
  let add_nodes graph nodes =
    Hashtbl.add graph (List.hd nodes) (List.tl nodes)
  in
  let graph = Hashtbl.create 2000 in
  List.iter (fun line -> add_nodes graph (get_nodes line)) lines;
  graph

let rec dfs node graph candidates =
  let rec recurse nodes graph candidates =
    match nodes with
    | [] -> ()
    | hd::tl -> dfs hd graph candidates; recurse tl graph candidates; Hashtbl.remove candidates hd;
  in
  if Hashtbl.mem candidates node then
    let children = Hashtbl.find graph node in
    recurse children graph candidates

let find_root graph =
  let candidates = Hashtbl.fold (fun k v c -> Hashtbl.add c k ""; c) graph (Hashtbl.create 2000) in
  let _ = Hashtbl.fold (fun k v (graph, candidates) -> dfs k graph candidates;(graph, candidates)) graph (graph, candidates) in
  Hashtbl.fold (fun k v _ -> print_endline k; k) candidates ""

let () =
  let lines = read_lines "input.txt" in
  let weights = build_weights lines in
  let graph = build_graph lines in
  let root = find_root graph in
  print_endline root