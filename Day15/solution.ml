let rec next_values inputA factorA inputB factorB =
  let next_value input factor = (input * factor) mod 2147483647 in
  ((next_value inputA factorA), (next_value inputB factorB))

let rec find_matching_pairs i inputA inputB res =
  if (i = 40000000) then res
  else
    let (inputA, inputB) = next_values inputA 16807 inputB 48271 in
    find_matching_pairs (i + 1) inputA inputB (res + if (inputA land 65535) = (inputB land 65535) then 1 else 0)

let () =
  let res = find_matching_pairs 0 703 516 0 in
  print_int res
