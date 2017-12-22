let rec rotate list idx out_list =
  if idx > 0 then
    let hd = List.hd list in
    rotate (List.tl list) (idx - 1) (out_list@[hd])
  else
    (out_list, list)

let spinlock steps num_iterations =
  let rec aux buffer steps num_iterations iteration =
    if iteration > num_iterations then
      buffer
    else
      let pos = steps mod (List.length buffer) in
      let (left, right) = rotate buffer (pos + 1) [] in
      let buffer = [iteration]@right@left in
      aux buffer steps num_iterations (iteration + 1)
   in
   let res = aux [0] steps num_iterations 1 in
   List.hd (List.tl res)

let () =
  print_int (spinlock 394 2017); print_endline ""


