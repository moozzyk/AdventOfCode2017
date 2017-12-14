let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec range mx curr l =
  if curr > mx then l else curr::range mx (curr + 1) l

let rec rotate list idx out_list =
  if idx > 0 then
    let hd = List.hd list in
    rotate (List.tl list) (idx - 1) (out_list@[hd])
  else
    (out_list, list)

let solve list lengths =
  let size = List.length list in
  let rec get_list list skip lengths hd_idx =
    match lengths with
      | [] -> let (left, right) = rotate list hd_idx [] in
        (right@left)
      | hd::tl ->
        let (left, right) = rotate list hd [] in
        let left = List.rev left in
        let list = left@right in
        let pivot = (skip + hd) mod size in
        let (left, right) = rotate list pivot [] in
        let hd_idx = if hd_idx <= pivot then hd_idx + (size - pivot) else hd_idx - pivot in
        get_list (right@left) (skip + 1) tl hd_idx
  in
  get_list list 0 lengths 0

let knot_hash line list =
  let chars = explode line in
  let lengths = List.map (fun c -> int_of_char c) chars in
  let lengths = (lengths@[17; 31; 73; 47; 23]) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let lengths = (lengths@lengths) in
  let out_list = solve list lengths in
  let rec fold_left list (idx, acc, res) =
    match list with
    | [] -> res
    | hd::tl ->
      if ((idx + 1) mod 16) = 0 then
        fold_left tl (idx + 1, 0, res@[(acc lxor hd)])
      else
        fold_left tl (idx + 1, acc lxor hd, res)
  in
  let compressed = fold_left out_list (0, 0, []) in
  let rec to_hash items hash =
    match items with
      | [] -> hash
      | hd::tl -> to_hash tl (Printf.sprintf "%s%02x" hash hd)
  in
    to_hash compressed ""

let solve input =
  let rec generate_inputs input num res =
    if num >= 128 then
      res
    else
      let s = Printf.sprintf("%s-%d") input num in
      let res = res@[s] in
      generate_inputs input (num + 1) res
  in
  let char_to_bin_str c =
    match c with
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'a' -> "1010"
    | 'b' -> "1011"
    | 'c' -> "1100"
    | 'd' -> "1101"
    | 'e' -> "1110"
    | 'f' -> "1111"
    | _ -> raise(Invalid_argument "")
  in
  let hash_to_bin_str h =
    let chars = explode h in
    let converted = List.map (fun c -> char_to_bin_str c) chars in
    String.concat "" converted
  in
  let inputs = generate_inputs input 0 [] in
  let hashes = List.map (fun i -> knot_hash i (range 255 0 [])) inputs in
  List.map (fun h -> hash_to_bin_str h) hashes

let rec visit_region board x y =
  if x >= 0 && y >= 0 && x <= 127 && y <= 127 then
    if board.(x).(y) = '1' then
    begin
      board.(x).(y) <- '0';
      visit_region board (x - 1) y;
      visit_region board (x + 1) y;
      visit_region board x (y - 1);
      visit_region board x (y + 1)
    end

let rec count_regions board pos num_regions =
  if pos >= 128 * 128
    then num_regions
    else
      let x = pos / 128 in
      let y = pos mod 128 in
      let num_regions = num_regions + if board.(x).(y) = '1' then 1 else 0 in
      visit_region board x y;
      count_regions board (pos + 1) num_regions

let () =
  let bin_strings = solve "ugkiagan" in
  List.iter (fun l -> print_endline l) bin_strings;
  let arrays = List.map (fun s -> Array.of_list (explode s)) bin_strings in
  (* Problem1 - dump output to a file and cound with find *)
  let matrix = Array.of_list arrays in
  print_endline "---------------------------------------------------------------------------------------------------------------------------";
  print_int (count_regions matrix 0 0); print_endline ""