let first_missing_positive nums =
  let n = Array.length nums in
  let i = ref 0 in
  while !i < n do
    let v = nums.(!i) in
    if v >= 1 && v <= n && nums.(v - 1) <> v then begin
      let tmp = nums.(!i) in
      nums.(!i) <- nums.(v - 1);
      nums.(v - 1) <- tmp
    end else
      incr i
  done;
  let ans = ref (n + 1) in
  let i = ref 0 in
  while !i < n && !ans = n + 1 do
    if nums.(!i) <> !i + 1 then ans := !i + 1;
    incr i
  done;
  !ans

let () =
  let rec read_all acc =
    try read_all (read_line () :: acc) with End_of_file -> List.rev acc
  in
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
                else match ys with
                  | z :: zs -> take (k - 1) zs (int_of_string z :: acc)
                  | [] -> failwith "bad input"
              in
              let nums, rems = take n tl [] in
              loop (tc + 1) rems (string_of_int (first_missing_positive nums) :: out)
          | [] -> List.rev out
      in
      print_string (String.concat "\n" (loop 0 rest []))
