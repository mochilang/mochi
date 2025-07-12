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


  type record1 = { mutable name : string; mutable age : int }
  type record2 = { mutable name : string; mutable age : int; mutable is_senior : bool }

let people : record1 list = [{ name = "Alice"; age = 30 };{ name = "Bob"; age = 15 };{ name = "Charlie"; age = 65 };{ name = "Diana"; age = 45 }]
let adults : record2 list = (let __res0 = ref [] in
  List.iter (fun (person : record1) ->
      if (person.age >= 18) then
    __res0 := { name = person.name; age = person.age; is_senior = (person.age >= 60) } :: !__res0;
  ) people;
List.rev !__res0)


let () =
  print_endline "--- Adults ---";
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | (person : record2)::rest ->
        try
          print_endline (__show (person.name) ^ " " ^ __show ("is") ^ " " ^ __show (person.age) ^ " " ^ __show ((if person.is_senior then " (senior)" else "")));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 adults with Break -> ()
