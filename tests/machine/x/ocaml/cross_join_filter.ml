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


  type record1 = { mutable n : int; mutable l : string }

let nums : int list = [1;2;3]
let letters : string list = ["A";"B"]
let pairs : (string * Obj.t) list list = (let __res0 = ref [] in
  List.iter (fun n ->
      List.iter (fun l ->
              if ((n mod 2) = 0) then
      __res0 := { n = n; l = l } :: !__res0;
      ) letters;
  ) nums;
List.rev !__res0)


let () =
  print_endline "--- Even pairs ---";
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | p::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "n" p)) ^ " " ^ __show (Obj.obj (List.assoc "l" p)));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 pairs with Break -> ()
