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

let build_graph lines =
  let graph = Hashtbl.create 2000 in
  let add_edges graph line =
    let parts = split line " <-> " in
    let child_nodes = split (List.hd (List.tl parts)) ", " in
    Hashtbl.add graph (List.hd parts) child_nodes
  in
  List.iter (fun l -> add_edges graph l) lines;
  graph

let solve_1 graph =
  let rec dfs graph node visited =
  if node = "0" then
    true
  else
    if Hashtbl.mem visited node then
      false
    else
    begin
      Hashtbl.add visited node "";
      let child_nodes = Hashtbl.find graph node in
      let res = List.map (fun n -> dfs graph n visited) child_nodes in
      List.fold_left (fun i acc -> i || acc) false res
    end
  in
  print_int (Hashtbl.fold (fun n v acc -> acc + if dfs graph n (Hashtbl.create 2000) then 1 else 0) graph 0); print_endline ""

let solve_2 graph =
  let rec dfs graph node visited =
    if Hashtbl.mem visited node then
      false
    else
    begin
      Hashtbl.add visited node "";
      let child_nodes = Hashtbl.find graph node in
      let res = List.map (fun n -> dfs graph n visited) child_nodes in
      List.fold_left (fun i acc -> i || acc) false res
    end
  in
  let fold_nodes graph node visited =
    let visited_before = Hashtbl.length visited in
    let _ = dfs graph node visited in
    if visited_before < (Hashtbl.length visited) then 1 else 0
  in
  let visited = Hashtbl.create 2000 in
  print_int (Hashtbl.fold (fun n v acc -> acc + fold_nodes graph n visited) graph 0); print_endline ""

let () =
  let lines = read_lines "input.txt" in
  let graph = build_graph lines in
  solve_1 graph;
  solve_2 graph;



