module Key = struct type t = int * int * int let equal = (=) let hash = Hashtbl.hash end
module H = Hashtbl.Make(Key)

let solve s1 s2 =
  let memo = H.create 128 in
  let rec dfs i1 i2 len =
    match H.find_opt memo (i1, i2, len) with
    | Some v -> v
    | None ->
        let a = String.sub s1 i1 len and b = String.sub s2 i2 len in
        let ans =
          if a = b then true
          else if List.sort compare (List.init len (fun i -> a.[i])) <> List.sort compare (List.init len (fun i -> b.[i])) then false
          else
            let rec loop k =
              k < len &&
              ((dfs i1 i2 k && dfs (i1 + k) (i2 + k) (len - k)) ||
               (dfs i1 (i2 + len - k) k && dfs (i1 + k) i2 (len - k)) ||
               loop (k + 1))
            in loop 1
        in H.add memo (i1, i2, len) ans; ans
  in dfs 0 0 (String.length s1)

let () =
  let rec read_all acc = try read_all (read_line () :: acc) with End_of_file -> List.rev acc in
  match read_all [] with
  | [] -> ()
  | t_str :: rest ->
      let t = int_of_string t_str in
      let rec run i xs acc =
        if i = t then List.rev acc else
        match xs with s1 :: s2 :: tl -> run (i + 1) tl ((if solve s1 s2 then "true" else "false") :: acc) | _ -> List.rev acc
      in print_string (String.concat "\n" (run 0 rest []))
