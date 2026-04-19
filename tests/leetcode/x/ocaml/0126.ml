module SMap = Map.Make(String)
module SSet = Set.Make(String)

let ladders begin_word end_word words =
  let word_set = List.fold_left (fun s w -> SSet.add w s) SSet.empty words in
  if not (SSet.mem end_word word_set) then [] else
  let rec bfs level visited parents found =
    if SSet.is_empty level || found then (parents, found) else
    let cur = List.sort compare (SSet.elements level) in
    let next, parents, found =
      List.fold_left (fun (next, parents, found) word ->
        let chars = Bytes.of_string word in
        let next, parents, found =
          List.fold_left (fun (next, parents, found) i ->
            let orig = Bytes.get chars i in
            let rec loop c (next, parents, found) =
              if c > Char.code 'z' then (next, parents, found) else
              let ch = Char.chr c in
              if ch = orig then loop (c + 1) (next, parents, found) else (
                Bytes.set chars i ch;
                let nw = Bytes.to_string chars in
                let acc =
                  if SSet.mem nw word_set && not (SSet.mem nw visited) then
                    let plist = match SMap.find_opt nw parents with Some v -> v | None -> [] in
                    (SSet.add nw next, SMap.add nw (word :: plist) parents, found || nw = end_word)
                  else (next, parents, found)
                in
                Bytes.set chars i orig;
                loop (c + 1) acc)
            in loop (Char.code 'a') (next, parents, found)
          ) (next, parents, found) (List.init (Bytes.length chars) Fun.id)
        in (next, parents, found)
      ) (SSet.empty, parents, false) cur
    in
    if found then (parents, true) else bfs next (SSet.union visited next) parents false
  in
  let parents, found = bfs (SSet.singleton begin_word) (SSet.singleton begin_word) SMap.empty false in
  if not found then [] else
  let rec backtrack word =
    if word = begin_word then [[begin_word]] else
    let plist = match SMap.find_opt word parents with Some v -> List.sort compare v | None -> [] in
    List.concat (List.map (fun p -> List.map (fun path -> path @ [word]) (backtrack p)) plist)
  in
  List.sort (fun a b -> compare (String.concat "->" a) (String.concat "->" b)) (backtrack end_word)

let fmt paths =
  String.concat "\n" (string_of_int (List.length paths) :: List.map (String.concat "->") paths)

let () =
  try
    let tc = int_of_string (read_line ()) in
    let out = ref [] in
    for _ = 1 to tc do
      let begin_word = read_line () in
      let end_word = read_line () in
      let n = int_of_string (read_line ()) in
      let words = List.init n (fun _ -> read_line ()) in
      out := fmt (ladders begin_word end_word words) :: !out
    done;
    print_string (String.concat "\n\n" (List.rev !out))
  with End_of_file -> ()
