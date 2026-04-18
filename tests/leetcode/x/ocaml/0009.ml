let read_all () =
  let buf = Buffer.create 1024 in
  (try while true do Buffer.add_string buf (input_line stdin); Buffer.add_char buf ' ' done with End_of_file -> ());
  Buffer.contents buf

let parse_ints s =
  s |> String.split_on_char ' ' |> List.map String.trim |> List.filter (fun x -> x <> "") |> List.map int_of_string

let is_palindrome x =
  if x < 0 then false
  else
    let original = x in
    let rec rev n acc =
      if n = 0 then acc else rev (n / 10) (acc * 10 + (n mod 10))
    in
    original = rev x 0

let () =
  match parse_ints (read_all ()) with
  | [] -> ()
  | t :: rest ->
      rest |> List.to_seq |> Seq.take t |> Seq.iter (fun x -> print_endline (if is_palindrome x then "true" else "false"))
