let get_permutation n k_input =
  let digits = ref (List.init n (fun i -> string_of_int (i + 1))) in
  let fact = Array.make (n + 1) 1 in
  for i = 1 to n do fact.(i) <- fact.(i - 1) * i done;
  let k = ref (k_input - 1) in
  let buf = Buffer.create n in
  for rem = n downto 1 do
    let block = fact.(rem - 1) in
    let idx = !k / block in
    k := !k mod block;
    let rec take i = function
      | [] -> ""
      | x :: xs -> if i = 0 then (digits := xs; x) else let y = take (i - 1) xs in digits := x :: !digits; y
    in
    let current = !digits in
    digits := [];
    Buffer.add_string buf (take idx current)
  done;
  Buffer.contents buf

let () =
  let rec read_all acc = try read_all (read_line () :: acc) with End_of_file -> List.rev acc in
  let lines = read_all [] in
  match lines with
  | [] -> ()
  | t_str :: rest ->
      let t = int_of_string (String.trim t_str) in
      let rec loop i xs =
        if i = 0 then []
        else match xs with
          | n_str :: k_str :: tl -> get_permutation (int_of_string (String.trim n_str)) (int_of_string (String.trim k_str)) :: loop (i - 1) tl
          | _ -> []
      in
      print_string (String.concat "\n" (loop t rest))
