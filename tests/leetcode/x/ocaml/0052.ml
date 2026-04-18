let solve n =
  let cols = Array.make n false and d1 = Array.make (2 * n) false and d2 = Array.make (2 * n) false in
  let board = Array.make n (String.make n '.') in
  let res = ref [] in
  let rec dfs r =
    if r = n then res := (Array.to_list (Array.map (fun s -> s) board)) :: !res
    else for c = 0 to n - 1 do let a = r + c and b = r - c + n - 1 in if not cols.(c) && not d1.(a) && not d2.(b) then begin cols.(c) <- true; d1.(a) <- true; d2.(b) <- true; board.(r) <- String.make c '.' ^ "Q" ^ String.make (n - c - 1) '.'; dfs (r + 1); board.(r) <- String.make n '.'; cols.(c) <- false; d1.(a) <- false; d2.(b) <- false end done
  in dfs 0; List.rev !res
let () =
  let rec read_all acc = try read_all (read_line () :: acc) with End_of_file -> List.rev acc in
  let lines = read_all [] in
  match lines with
  | [] -> ()
  | t_str :: rest ->
      let t = int_of_string (String.trim t_str) in
      let rec loop tc xs =
        if tc = t then []
        else
          let n = int_of_string (String.trim (List.hd xs)) in
          let sols = solve n in
          let block = [ string_of_int (List.length sols) ] in
          block @ loop (tc + 1) (List.tl xs)
      in
      print_string (String.concat "\n" (loop 0 rest))
