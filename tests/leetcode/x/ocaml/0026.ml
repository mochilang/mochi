let rec remove_duplicates = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: rest ->
      if x = y then remove_duplicates (y :: rest)
      else x :: remove_duplicates (y :: rest)

let rec take n acc =
  if n = 0 then List.rev acc
  else
    let v = Scanf.scanf " %d" (fun x -> x) in
    take (n - 1) (v :: acc)

let solve () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t do
      let n = Scanf.scanf " %d" (fun x -> x) in
      let nums = take n [] in
      let ans = remove_duplicates nums in
      print_endline (String.concat " " (List.map string_of_int ans))
    done
  with End_of_file -> ()

let () = solve ()
