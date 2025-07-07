let nums = [1; 2; 3]
let result = List.fold_left (fun acc n -> if n > 1 then acc + n else acc) 0 nums

let () =
  Printf.printf "%d\n" result
