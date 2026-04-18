let trap h =
  let left = ref 0 and right = ref (Array.length h - 1) and left_max = ref 0 and right_max = ref 0 and water = ref 0 in
  while !left <= !right do
    if !left_max <= !right_max then begin
      let v = h.(!left) in
      if v < !left_max then water := !water + !left_max - v else left_max := v;
      incr left
    end else begin
      let v = h.(!right) in
      if v < !right_max then water := !water + !right_max - v else right_max := v;
      decr right
    end
  done;
  !water

let () =
  let rec read_all acc = try read_all (read_line () :: acc) with End_of_file -> List.rev acc in
  let lines = List.map String.trim (read_all []) in
  match lines with
  | [] -> ()
  | t_str :: rest ->
      let t = int_of_string t_str in
      let rec loop tc xs out =
        if tc = t then List.rev out else
        match xs with
        | n_str :: tl ->
            let n = int_of_string n_str in
            let rec take k ys acc =
              if k = 0 then (Array.of_list (List.rev acc), ys)
              else match ys with z :: zs -> take (k - 1) zs (int_of_string z :: acc) | [] -> failwith "bad input"
            in
            let arr, rems = take n tl [] in
            loop (tc + 1) rems (string_of_int (trap arr) :: out)
        | [] -> List.rev out
      in
      print_string (String.concat "\n" (loop 0 rest []))
