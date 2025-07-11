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

  let load_yaml path =
    let ic = if path = "-" then stdin else open_in path in
    let rec parse acc cur =
      try
        let line = String.trim (input_line ic) in
        if line = "" then parse acc cur else
        if String.get line 0 = '-' then (
          let acc = (match cur with None -> acc | Some m -> m :: acc) in
          let l = String.trim (String.sub line 1 (String.length line - 1)) in
          let idx = String.index l ':' in
          let key = String.sub l 0 idx |> String.trim in
          let value = String.sub l (idx+1) (String.length l - idx - 1) |> String.trim in
          let cur = Some [ (key, Obj.repr value) ] in
          parse acc cur
        ) else (
          let idx = String.index line ':' in
          let key = String.sub line 0 idx |> String.trim in
          let value = String.sub line (idx+1) (String.length line - idx - 1) |> String.trim in
          let v = Obj.repr value in
          let cur = match cur with None -> Some [ (key,v) ] | Some m -> Some ((key,v)::m) in
          parse acc cur
        )
      with End_of_file ->
        if path <> "-" then close_in ic;
        let acc = match cur with None -> acc | Some m -> m :: acc in
        List.rev acc
    in parse [] None


  type record1 = { mutable name : string; mutable email : string }

type person = { mutable name : string; mutable age : int; mutable email : string }
let people : person list = List.map (fun m -> { name = (Obj.obj (List.assoc "name" m) : string); age = (Obj.obj (List.assoc "age" m) : int); email = (Obj.obj (List.assoc "email" m) : string) }) (load_yaml "../interpreter/valid/people.yaml")
let adults : record1 list = (let __res0 = ref [] in
  List.iter (fun p ->
      if (p.age >= 18) then
    __res0 := { name = p.name; email = p.email } :: !__res0;
  ) people;
List.rev !__res0)


let () =
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | a::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "name" a)) ^ " " ^ __show (Obj.obj (List.assoc "email" a)));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 adults with Break -> ()
