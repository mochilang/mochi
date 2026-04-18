let longest s =
  let last = Hashtbl.create 128 in
  let left = ref 0 in
  let best = ref 0 in
  for right = 0 to String.length s - 1 do
    let ch = s.[right] in
    (match Hashtbl.find_opt last ch with Some pos when pos >= !left -> left := pos + 1 | _ -> ());
    Hashtbl.replace last ch right;
    best := max !best (right - !left + 1)
  done;
  !best

let read_all () =
  let rec aux acc = try aux (input_line stdin :: acc) with End_of_file -> List.rev acc in aux []

let () =
  match read_all () with
  | [] -> ()
  | first :: rest ->
      let t = int_of_string (String.trim first) in
      print_string (String.concat "\n" (List.map (fun s -> string_of_int (longest s)) (List.filteri (fun i _ -> i < t) rest)))
