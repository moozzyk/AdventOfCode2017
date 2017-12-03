(* Problem 1 - solved manually *)
let solve input =
    let tmp = int_of_float (sqrt (float_of_int (input - 1))) in
    let side = if tmp land 1 = 1 then tmp + 1 else tmp in
    let pos = (input - (side - 1) * (side - 1)) mod side in
    if pos < side / 2 then side - pos else pos

(* Problem 2 *)
let fill_ring matrix size ring =
    let sum_neighbors matrix row col =
        matrix.(row - 1).(col) +
        matrix.(row - 1).(col - 1) +
        matrix.(row - 1).(col + 1) +
        matrix.(row + 1).(col) +
        matrix.(row + 1).(col - 1) +
        matrix.(row + 1).(col + 1) +
        matrix.(row).(col - 1) +
        matrix.(row).(col + 1) in

    let center = size / 2 in
    for i = 0 to ring * 2 - 1 do
        let row = center + ring - 1 - i in
        let col = center + ring in
        matrix.(row).(col) <- sum_neighbors matrix row col
    done;
    for i = 0 to ring * 2 - 1 do
        let row = center - ring in
        let col = center + ring - 1 - i in
        matrix.(row).(col) <- sum_neighbors matrix row col
    done;
    for i = 0 to ring * 2 - 1 do
        let row = center - ring + 1 + i in
        let col = center - ring in
        matrix.(row).(col) <- sum_neighbors matrix row col
    done;
    for i = 0 to ring * 2 - 1 do
        let row = center + ring in
        let col = center - ring + 1 + i in
        matrix.(row).(col) <- sum_neighbors matrix row col
    done

let print_matrix matrix size =
    for row = 0 to size - 1 do
        for col = 0 to size - 1 do
            print_int matrix.(row).(col); print_string " "
        done;
        print_endline ""
    done

let () =
    print_int (solve 277678); print_endline "";

    let size = 11 in (* size has to be odd *)
    let matrix = Array.make_matrix size size 0 in
    matrix.(size / 2).(size / 2) <- 1;
    for ring = 1 to 4 do
        fill_ring matrix size ring
    done;
    print_matrix matrix size;
