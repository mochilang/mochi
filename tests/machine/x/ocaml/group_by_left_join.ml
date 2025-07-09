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

let customers = [[("id",1);("name","Alice")];[("id",2);("name","Bob")];[("id",3);("name","Charlie")]]
let orders = [[("id",100);("customerId",1)];[("id",101);("customerId",1)];[("id",102);("customerId",2)]]
let stats = (let __res0 = ref [] in
  List.iter (fun c ->
      __res0 := [("name",g.key);("count",List.length (let __res1 = ref [] in
  List.iter (fun r ->
      if r.o then
    __res1 := r :: !__res1;
  ) g;
List.rev !__res1)
)] :: !__res0;
  ) customers;
List.rev !__res0)


let () =
  print_endline (__show ("--- Group Left Join ---"));
  let rec __loop2 lst =
    match lst with
      | [] -> ()
      | s::rest ->
        try
          print_endline (__show (s.name) ^ " " ^ __show ("orders:") ^ " " ^ __show (s.count));
        with Continue -> ()
        ; __loop2 rest
    in
    try __loop2 stats with Break -> ()
