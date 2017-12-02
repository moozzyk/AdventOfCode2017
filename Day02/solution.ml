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

let solve int_lists =
    let rec min_max_diff l minimum maximum =
        match l with
        | [] -> maximum - minimum
        | h::t -> min_max_diff t (min h minimum) (max h maximum) in
  let diffs = List.map (fun item -> min_max_diff item (List.nth item 0) (List.nth item 0)) int_lists in
  List.fold_left (+) 0 diffs

let() =
    let preprocess string =
        let str_array = split string "\t" in
        List.map (fun item -> int_of_string item) str_array in
    let lines = read_lines "input.txt" in
    let int_lists = List.map (fun item -> preprocess item) lines in
    print_int (solve int_lists); print_endline ""
