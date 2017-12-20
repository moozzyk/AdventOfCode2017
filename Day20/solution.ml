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

let print_list l =
  List.iter (fun i -> print_string i; print_string "*") l

let get_matches line regex  =
  let rec get_matches_aux regex line start out =
    try let start = Str.search_forward regex line start in
      let m = Str.matched_string line in
      let out = out@[m] in
      get_matches_aux regex line (start + String.length m + 1) out
    with _ -> out
  in
  get_matches_aux regex line 0 []

type triplet = { mutable x: int; mutable y: int; mutable z: int; }
type particle = { position: triplet; velocity: triplet; acceleration: triplet }

let print_particle p =
  let print_triplet t =
    Printf.printf "<%d, %d, %d>" t.x t.y t.z
  in
  print_string "p: ";print_triplet p.position;
  print_string " v: ";print_triplet p.velocity;
  print_string " a: ";print_triplet p.acceleration

let load_particles lines =
  let regex = Str.regexp "-?[0-9]+" in
  let process_matches l = Array.of_list (List.map (fun i -> int_of_string(i)) l) in
  let numbers = List.map (fun l -> process_matches(get_matches l regex)) lines in
  List.map (fun n ->
    {
      position = { x = n.(0); y = n.(1); z = n.(2) };
      velocity = { x = n.(3); y = n.(4); z = n.(5) };
      acceleration = { x = n.(6); y = n.(7); z = n.(8) }
    }) numbers

let tick particles =
  let move particle =
    particle.velocity.x <- particle.velocity.x + particle.acceleration.x;
    particle.velocity.y <- particle.velocity.y + particle.acceleration.y;
    particle.velocity.z <- particle.velocity.z + particle.acceleration.z;
    particle.position.x <- particle.position.x + particle.velocity.x;
    particle.position.y <- particle.position.y + particle.velocity.y;
    particle.position.z <- particle.position.z + particle.velocity.z
  in
  List.iter (fun p -> move p) particles

let solve particles =
  let abs_triplet t = (abs t.x) + (abs t.y) + (abs t.z) in
  let rec aux particles i min_i min_p =
    match particles with
    | [] -> min_i
    | hd::tl ->
      let min_acc = abs_triplet min_p.acceleration in
      let curr_acc = abs_triplet hd.acceleration in
      let min_vel = abs_triplet min_p.velocity in
      let curr_vel = abs_triplet hd.velocity in
      let min_pos = abs_triplet min_p.position in
      let curr_pos = abs_triplet hd.position in

      if (curr_acc < min_acc) ||
         (curr_acc = min_acc && curr_vel < min_vel) ||
         (curr_acc = min_acc && curr_vel = min_vel && curr_pos < min_pos) then
        aux tl (i + 1) i hd
      else
        aux tl (i + 1) min_i min_p
  in
  aux particles 0 0 (List.hd particles)

let () =
  let lines = read_lines "input.txt" in
  let particles = load_particles lines in
  print_int (solve particles)
