let solve a =
  let n = Array.length a in
  let best = ref 0 in
  for i = 0 to n - 1 do
    let mn = ref a.(i) in
    for j = i to n - 1 do
      if a.(j) < !mn then mn := a.(j);
      let area = !mn * (j - i + 1) in
      if area > !best then best := area
    done
  done;
  !best

let () =
  let rec read_all acc = try read_all (read_line () :: acc) with End_of_file -> List.rev acc in
  match read_all [] with
  | [] -> ()
  | t_str :: rest ->
      let t = int_of_string t_str in
      let rec run rem xs acc =
        if rem = 0 then List.rev acc
        else
          match xs with
          | n_str :: tl ->
              let n = int_of_string n_str in
              let rec take k ys got =
                if k = 0 then (Array.of_list (List.rev got), ys)
                else match ys with y :: ys2 -> take (k - 1) ys2 (int_of_string y :: got) | [] -> (Array.of_list (List.rev got), [])
              in
              let arr, tail = take n tl [] in
              run (rem - 1) tail (string_of_int (solve arr) :: acc)
          | [] -> List.rev acc
      in
      print_string (String.concat "\n" (run t rest []))
