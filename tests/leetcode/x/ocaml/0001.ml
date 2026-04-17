let read_all () =
  let buf = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_string buf (input_line stdin);
       Buffer.add_char buf ' '
     done
   with End_of_file -> ());
  Buffer.contents buf

let parse_ints s =
  let normalized =
    String.map
      (fun c -> if c = '\n' || c = '\r' || c = '\t' then ' ' else c)
      s
  in
  normalized
  |> String.split_on_char ' '
  |> List.filter (fun x -> x <> "")
  |> List.map int_of_string
  |> Array.of_list

let two_sum nums target =
  let n = Array.length nums in
  let rec outer i =
    if i >= n then (0, 0)
    else
      let rec inner j =
        if j >= n then outer (i + 1)
        else if nums.(i) + nums.(j) = target then (i, j)
        else inner (j + 1)
      in
      inner (i + 1)
  in
  outer 0

let () =
  let values = parse_ints (read_all ()) in
  if Array.length values > 0 then (
    let t = values.(0) in
    let idx = ref 1 in
    for tc = 0 to t - 1 do
      let n = values.(!idx) in
      incr idx;
      let target = values.(!idx) in
      incr idx;
      let nums = Array.init n (fun i -> values.(!idx + i)) in
      idx := !idx + n;
      let a, b = two_sum nums target in
      Printf.printf "%d %d" a b;
      if tc + 1 < t then print_newline ()
    done)
