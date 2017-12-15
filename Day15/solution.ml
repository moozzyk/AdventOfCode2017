let rec next_value input factor mod_val =
  let res = (input * factor) mod 2147483647 in
  if (res mod mod_val) = 0 then res else next_value res factor mod_val

let rec find_matching_pairs i (inputA, factorA, modA) (inputB, factorB, modB) res =
  if (i = 0) then res
  else
    let inputA = next_value inputA factorA modA in
    let inputB = next_value inputB factorB modB in
    let res = res + if (inputA land 65535) = (inputB land 65535) then 1 else 0 in
    find_matching_pairs (i - 1) (inputA, factorA, modA) (inputB, factorB, modB) res

let () =
  let inputA = 703 in
  let inputB = 516 in
  print_int (find_matching_pairs 40000000 (inputA, 16807, 1) (inputB, 48271, 1) 0); print_endline "";
  print_int (find_matching_pairs 5000000 (inputB, 16807, 4) (inputB, 48271, 8) 0); print_endline "";
