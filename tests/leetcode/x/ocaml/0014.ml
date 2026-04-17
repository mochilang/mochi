let starts_with s p =
  String.length p <= String.length s && String.sub s 0 (String.length p) = p

let lcp strs =
  let rec loop p =
    if List.for_all (fun s -> starts_with s p) strs then p
    else loop (String.sub p 0 (String.length p - 1))
  in
  loop (List.hd strs)

let read_all () =
  let buf = Buffer.create 1024 in
  (try while true do Buffer.add_string buf (input_line stdin); Buffer.add_char buf ' ' done with End_of_file -> ());
  Buffer.contents buf

let () =
  let tokens = String.split_on_char ' ' (read_all ()) |> List.map String.trim |> List.filter (fun x -> x <> "") in
  match tokens with
  | [] -> ()
  | t :: rest ->
      let rec solve vals left =
        if left = 0 then () else
        match vals with
        | n :: tail ->
            let k = int_of_string n in
            let strs = List.filteri (fun i _ -> i < k) tail in
            print_endline ("\"" ^ lcp strs ^ "\"");
            solve (List.filteri (fun i _ -> i >= k) tail) (left - 1)
        | [] -> ()
      in solve rest (int_of_string t)
