let read_all ic =
  let b = Buffer.create 1024 in
  (try while true do Buffer.add_channel b ic 1 done with End_of_file -> ());
  Buffer.contents b

let () =
  let lines = String.split_on_char '
' (read_all stdin) in
  match lines with
  | [] | [""] -> ()
  | _ ->
      let arr = Array.of_list lines in
      let idx = ref 0 in
      let next () = let v = if !idx < Array.length arr then arr.(!idx) else "0" in incr idx; String.trim v in
      let t = int_of_string (next ()) in
      let out = ref [] in
      for _ = 1 to t do
        let k = int_of_string (next ()) in
        let vals = ref [] in
        for _ = 1 to k do
          let n = int_of_string (next ()) in
          for _ = 1 to n do vals := int_of_string (next ()) :: !vals done
        done;
        let vals = List.sort compare !vals in
        out := !out @ ["[" ^ String.concat "," (List.map string_of_int vals) ^ "]"]
      done;
      print_string (String.concat "\n" !out)
