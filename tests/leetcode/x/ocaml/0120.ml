let solve tri =
  let rev_tri = List.rev tri in
  let dp = Array.of_list (List.hd rev_tri) in
  let rows = List.tl rev_tri in
  List.iter (fun row ->
    List.iteri (fun j v -> dp.(j) <- v + min dp.(j) dp.(j + 1)) row
  ) rows;
  dp.(0)

let () =
  let rec read_all acc =
    try read_all (int_of_string (read_line ()) :: acc) with End_of_file -> List.rev acc
  in
  match read_all [] with
  | [] -> ()
  | t :: rest ->
      let rec read_row k need xs row =
        if k = need then
          (List.rev row, xs)
        else
          match xs with
          | y :: ys -> read_row (k + 1) need ys (y :: row)
          | [] -> (List.rev row, [])
      in
      let rec read_rows r rows xs tri =
        if r > rows then
          (List.rev tri, xs)
        else
          let row, xs2 = read_row 0 r xs [] in
          read_rows (r + 1) rows xs2 (row :: tri)
      in
      let rec run tc xs acc =
        if tc = t then
          List.rev acc
        else
          match xs with
          | rows :: tl ->
              let tri, tl2 = read_rows 1 rows tl [] in
              run (tc + 1) tl2 (string_of_int (solve tri) :: acc)
          | [] -> List.rev acc
      in
      print_string (String.concat "\n" (run 0 rest []))
