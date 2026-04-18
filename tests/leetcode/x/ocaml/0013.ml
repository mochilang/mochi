let value = function
  | 'I' -> 1 | 'V' -> 5 | 'X' -> 10 | 'L' -> 50 | 'C' -> 100 | 'D' -> 500 | 'M' -> 1000 | _ -> 0

let roman_to_int s =
  let n = String.length s in
  let rec loop i acc =
    if i >= n then acc
    else
      let cur = value s.[i] in
      let next = if i + 1 < n then value s.[i + 1] else 0 in
      loop (i + 1) (acc + if cur < next then -cur else cur)
  in
  loop 0 0

let read_all () =
  let buf = Buffer.create 1024 in
  (try while true do Buffer.add_string buf (input_line stdin); Buffer.add_char buf ' ' done with End_of_file -> ());
  Buffer.contents buf

let () =
  match String.split_on_char ' ' (read_all ()) |> List.map String.trim |> List.filter (fun x -> x <> "") with
  | [] -> ()
  | t :: rest -> rest |> List.to_seq |> Seq.take (int_of_string t) |> Seq.iter (fun s -> print_endline (string_of_int (roman_to_int s)))
