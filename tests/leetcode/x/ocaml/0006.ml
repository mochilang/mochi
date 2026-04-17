let convert_zigzag s num_rows =
  let n = String.length s in
  if num_rows <= 1 || num_rows >= n then s
  else
    let cycle = 2 * num_rows - 2 in
    let out = Buffer.create n in
    for row = 0 to num_rows - 1 do
      let rec loop i =
        if i < n then begin
          Buffer.add_char out s.[i];
          let diag = i + cycle - 2 * row in
          if row > 0 && row < num_rows - 1 && diag < n then Buffer.add_char out s.[diag];
          loop (i + cycle)
        end
      in
      loop row
    done;
    Buffer.contents out

let () =
  try
    let t = int_of_string (String.trim (read_line ())) in
    let out = ref [] in
    for _ = 1 to t do
      let s = read_line () in
      let r = int_of_string (String.trim (read_line ())) in
      out := convert_zigzag s r :: !out
    done;
    print_string (String.concat "\n" (List.rev !out))
  with End_of_file -> ()
