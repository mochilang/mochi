let matches close_br open_br =
  (close_br = ')' && open_br = '(') ||
  (close_br = ']' && open_br = '[') ||
  (close_br = '}' && open_br = '{')

let is_valid s =
  let n = String.length s in
  let rec loop i stack =
    if i >= n then stack = []
    else
      let ch = s.[i] in
      if ch = '(' || ch = '[' || ch = '{' then
        loop (i + 1) (ch :: stack)
      else
        match stack with
        | open_br :: rest when matches ch open_br -> loop (i + 1) rest
        | _ -> false
  in
  loop 0 []

let read_all () =
  let buf = Buffer.create 1024 in
  (try while true do Buffer.add_string buf (input_line stdin); Buffer.add_char buf ' ' done with End_of_file -> ());
  Buffer.contents buf

let () =
  match String.split_on_char ' ' (read_all ()) |> List.map String.trim |> List.filter (fun x -> x <> "") with
  | [] -> ()
  | t :: rest ->
      rest |> List.to_seq |> Seq.take (int_of_string t)
      |> Seq.iter (fun s -> print_endline (if is_valid s then "true" else "false"))
