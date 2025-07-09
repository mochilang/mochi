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

let string_contains s sub =
  let len_s = String.length s and len_sub = String.length sub in
  let rec aux i =
    if i + len_sub > len_s then false
    else if String.sub s i len_sub = sub then true
    else aux (i + 1)
  in aux 0

let slice lst i j =
  lst |> List.mapi (fun idx x -> idx, x)
      |> List.filter (fun (idx, _) -> idx >= i && idx < j)
      |> List.map snd

let string_slice s i j = String.sub s i (j - i)

let people = [[("name","Alice");("age",30);("city","Paris")];[("name","Bob");("age",15);("city","Hanoi")];[("name","Charlie");("age",65);("city","Paris")];[("name","Diana");("age",45);("city","Hanoi")];[("name","Eve");("age",70);("city","Paris")];[("name","Frank");("age",22);("city","Hanoi")]]
let stats = (let __res0 = ref [] in
  List.iter (fun person ->
      __res0 := [("city",g.key);("count",List.length g);("avg_age",(List.fold_left (+) 0 (let __res1 = ref [] in
  List.iter (fun p ->
      __res1 := p.age :: !__res1;
  ) g;
List.rev !__res1)
 / List.length (let __res1 = ref [] in
  List.iter (fun p ->
      __res1 := p.age :: !__res1;
  ) g;
List.rev !__res1)
))] :: !__res0;
  ) people;
List.rev !__res0)


let () =
  print_endline (__show ("--- People grouped by city ---"));
  let rec __loop2 lst =
    match lst with
      | [] -> ()
      | s::rest ->
        try
          print_endline (__show (s.city) ^ " " ^ __show (": count =") ^ " " ^ __show (s.count) ^ " " ^ __show (", avg_age =") ^ " " ^ __show (s.avg_age));
        with Continue -> ()
        ; __loop2 rest
    in
    try __loop2 stats with Break -> ()
