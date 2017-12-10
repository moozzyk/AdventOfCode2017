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
  List.iter (fun line -> add_weight weights line) lines;
  weights

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

let rec all_same list previous =
  match list with
  | [] -> true
  | hd::tl -> if previous < 0 || hd = previous then all_same tl hd else false

let rec find_balance node graph weights =
  let children = Hashtbl.find graph node in
  let node_weight = Hashtbl.find weights node in
  let children_weights = List.map (fun n -> find_balance n graph weights) children in
  let balanced = all_same children_weights min_int in
  if balanced = false then
  begin
    print_endline node;
    List.iter (fun w -> print_int w; print_string " ") children_weights;
    print_endline "";
    List.iter (fun n -> print_int (Hashtbl.find weights n); print_string " ") children;
    print_endline ""
  end;
  node_weight + (List.fold_left (fun w acc -> w + acc) 0 children_weights)

let () =
  let lines = read_lines "input.txt" in
  let weights = build_weights lines in
  let graph = build_graph lines in
  let root = find_root graph in
  print_endline root; print_endline "";
  (* in the first unbalanced list subtract the weight of unbalanced node from the sum of weights of node and its descendants *)
  let _ = find_balance root graph weights in
  print_endline "";