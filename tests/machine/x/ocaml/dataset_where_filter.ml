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


let people = [[("name",Obj.repr ("Alice"));("age",Obj.repr (30))];[("name",Obj.repr ("Bob"));("age",Obj.repr (15))];[("name",Obj.repr ("Charlie"));("age",Obj.repr (65))];[("name",Obj.repr ("Diana"));("age",Obj.repr (45))]]
let adults = (let __res0 = ref [] in
  List.iter (fun person ->
      if (Obj.obj (List.assoc "age" person) >= 18) then
    __res0 := [("name",Obj.repr (Obj.obj (List.assoc "name" person)));("age",Obj.repr (Obj.obj (List.assoc "age" person)));("is_senior",Obj.repr ((Obj.obj (List.assoc "age" person) >= 60)))] :: !__res0;
  ) people;
List.rev !__res0)


let () =
  print_endline (__show ("--- Adults ---"));
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | person::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "name" person)) ^ " " ^ __show ("is") ^ " " ^ __show (Obj.obj (List.assoc "age" person)) ^ " " ^ __show ((if Obj.obj (List.assoc "is_senior" person) then " (senior)" else "")));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 adults with Break -> ()
