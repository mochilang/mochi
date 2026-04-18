let hist a =
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
        if rem = 0 then List.rev acc else
        match xs with
        | rc :: tl ->
            let parts = String.split_on_char ' ' rc |> List.filter (fun s -> s <> "") in
            let rows = int_of_string (List.hd parts) in
            let cols = int_of_string (List.hd (List.tl parts)) in
            let h = Array.make cols 0 in
            let rec step r ys best =
              if r = 0 then (best, ys) else
              match ys with
              | s :: ys2 ->
                  for c = 0 to cols - 1 do h.(c) <- if s.[c] = '1' then h.(c) + 1 else 0 done;
                  step (r - 1) ys2 (max best (hist h))
              | [] -> (best, [])
            in
            let best, tail = step rows tl 0 in
            run (rem - 1) tail (string_of_int best :: acc)
        | [] -> List.rev acc
      in
      print_string (String.concat "\n" (run t rest []))
