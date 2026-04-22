let solve dungeon =
  let cols = List.length (List.hd dungeon) in
  let inf = 1 lsl 60 in
  let dp = Array.make (cols + 1) inf in
  dp.(cols - 1) <- 1;
  List.iter (fun row ->
    for j = cols - 1 downto 0 do
      let need = min dp.(j) dp.(j + 1) - List.nth row j in
      dp.(j) <- if need <= 1 then 1 else need
    done
  ) (List.rev dungeon);
  dp.(0)

let () =
  let toks = List.filter (fun s -> s <> "") (String.split_on_char ' ' (String.map (fun c -> if c = '\n' || c = '\r' || c = '\t' then ' ' else c) (In_channel.input_all stdin))) in
  match toks with
  | [] -> ()
  | t_str :: rest ->
      let t = int_of_string t_str in
      let rec read_rows rows cols xs acc =
        if rows = 0 then (List.rev acc, xs)
        else
          let rec take_row n ys row =
            if n = 0 then (List.rev row, ys)
            else match ys with
              | z :: zs -> take_row (n - 1) zs (int_of_string z :: row)
              | [] -> (List.rev row, [])
          in
          let row, tail = take_row cols xs [] in
          read_rows (rows - 1) cols tail (row :: acc)
      in
      let rec run n xs acc =
        if n = 0 then List.rev acc
        else match xs with
          | rows :: cols :: tl ->
              let dungeon, tl2 = read_rows (int_of_string rows) (int_of_string cols) tl [] in
              run (n - 1) tl2 (string_of_int (solve dungeon) :: acc)
          | _ -> List.rev acc
      in
      print_string (String.concat "\n" (run t rest []))
