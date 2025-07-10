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

type ('k,'v) group = { key : 'k; items : 'v list }

let people = [[("name",Obj.repr ("Alice"));("age",Obj.repr (30));("city",Obj.repr ("Paris"))];[("name",Obj.repr ("Bob"));("age",Obj.repr (15));("city",Obj.repr ("Hanoi"))];[("name",Obj.repr ("Charlie"));("age",Obj.repr (65));("city",Obj.repr ("Paris"))];[("name",Obj.repr ("Diana"));("age",Obj.repr (45));("city",Obj.repr ("Hanoi"))];[("name",Obj.repr ("Eve"));("age",Obj.repr (70));("city",Obj.repr ("Paris"))];[("name",Obj.repr ("Frank"));("age",Obj.repr (22));("city",Obj.repr ("Hanoi"))]]
let stats = (let __groups0 = ref [] in
  List.iter (fun person ->
      let key = Obj.obj (List.assoc "city" person) in
      let cur = try List.assoc key !__groups0 with Not_found -> [] in
      __groups0 := (key, person :: cur) :: List.remove_assoc key !__groups0;
  ) people;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := [("city",Obj.repr (g.key));("count",Obj.repr (List.length g));("avg_age",Obj.repr ((List.fold_left (+) 0 (let __res1 = ref [] in
  List.iter (fun p ->
      __res1 := p.age :: !__res1;
  ) g;
List.rev !__res1)
 / List.length (let __res1 = ref [] in
  List.iter (fun p ->
      __res1 := p.age :: !__res1;
  ) g;
List.rev !__res1)
)))] :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline (__show ("--- People grouped by city ---"));
  let rec __loop2 lst =
    match lst with
      | [] -> ()
      | s::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "city" s)) ^ " " ^ __show (": count =") ^ " " ^ __show (Obj.obj (List.assoc "count" s)) ^ " " ^ __show (", avg_age =") ^ " " ^ __show (Obj.obj (List.assoc "avg_age" s)));
        with Continue -> ()
        ; __loop2 rest
    in
    try __loop2 stats with Break -> ()
