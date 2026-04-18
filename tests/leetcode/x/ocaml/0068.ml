let justify words max_w =
  let rec take_line ws total acc =
    match ws with
    | [] -> (List.rev acc, [])
    | w :: rest ->
        let cnt = List.length acc in
        if total + String.length w + cnt <= max_w then take_line rest (total + String.length w) (w :: acc)
        else (List.rev acc, ws)
  in
  let rec build ws =
    match ws with
    | [] -> []
    | _ ->
        let line_words, rest = take_line ws 0 [] in
        let total = List.fold_left (fun a w -> a + String.length w) 0 line_words in
        let gaps = List.length line_words - 1 in
        let line =
          if rest = [] || gaps = 0 then
            let s = String.concat " " line_words in
            s ^ String.make (max_w - String.length s) ' '
          else
            let spaces = max_w - total and base = (max_w - total) / gaps and extra = (max_w - total) mod gaps in
            let rec spread ws extra =
              match ws with
              | [] -> ""
              | [w] -> w
              | w :: tl -> w ^ String.make (base + if extra > 0 then 1 else 0) ' ' ^ spread tl (max 0 (extra - 1))
            in
            let _ = spaces in spread line_words extra
        in
        line :: build rest
  in build words
let () =
  let rec read_all acc = try read_all (read_line () :: acc) with End_of_file -> List.rev acc in
  match read_all [] with
  | [] -> ()
  | t_str :: rest ->
      let t = int_of_string t_str in
      let rec solve tc xs =
        if tc = 0 then [] else
        let n = int_of_string (List.hd xs) in
        let words = List.filteri (fun i _ -> i < n) (List.tl xs) in
        let width = int_of_string (List.nth xs (n + 1)) in
        let ans = justify words width in
        (string_of_int (List.length ans)) :: List.map (fun s -> "|" ^ s ^ "|") ans @ (if tc > 1 then ["="] else []) @ solve (tc - 1) (List.drop (n + 2) xs)
      in print_string (String.concat "\n" (solve t rest))
