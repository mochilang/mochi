let rec __show v =
  let open Obj in
  let rec list_aux o =
    if is_int o && (magic (obj o) : int) = 0 then "" else
     let hd = field o 0 in
     let tl = field o 1 in
     let rest = list_aux tl in
     if rest = "" then __show (obj hd) else __show (obj hd) ^ "; " ^ rest
  in
  let r = repr v in
  if is_int r then string_of_int (magic v) else
  match tag r with
    | 0 -> if size r = 0 then "[]" else "[" ^ list_aux r ^ "]"
    | 252 -> (magic v : string)
    | 253 -> string_of_float (magic v)
    | _ -> "<value>"

exception Break
exception Continue


let rec twoSum (nums : int list) (target : int) : int list =
  let n = List.length nums in
  let rec __loop0 i =
    if i > n then () else (
      try
        let i = i in
        let rec __loop1 i =
          if i > n then () else (
            try
              let j = i in
              if ((List.nth nums i + List.nth nums j) = target) then (
                [i;j]
              ) ;
            with Continue -> ()
            ; __loop1 (i + 1))
        in
        try __loop1 (i + 1) with Break -> ()
      with Continue -> ()
      ; __loop0 (i + 1))
  in
  try __loop0 0 with Break -> ()
  [-1;-1]

let result = twoSum [2;7;11;15] 9

let () =
  print_endline (__show (List.nth result 0));
  print_endline (__show (List.nth result 1));
