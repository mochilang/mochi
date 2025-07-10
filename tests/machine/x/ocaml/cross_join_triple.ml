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


  type record1 = { mutable n : int; mutable l : string; mutable b : bool }

let nums : int list = [1;2]
let letters : string list = ["A";"B"]
let bools : bool list = [true;false]
let combos : (string * Obj.t) list list = (let __res0 = ref [] in
  List.iter (fun n ->
      List.iter (fun l ->
            List.iter (fun b ->
                        __res0 := { n = n; l = l; b = b } :: !__res0;
            ) bools;
      ) letters;
  ) nums;
List.rev !__res0)


let () =
  print_endline "--- Cross Join of three lists ---";
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | c::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "n" c)) ^ " " ^ __show (Obj.obj (List.assoc "l" c)) ^ " " ^ __show (Obj.obj (List.assoc "b" c)));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 combos with Break -> ()
