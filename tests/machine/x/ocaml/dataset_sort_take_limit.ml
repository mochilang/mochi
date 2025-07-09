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

let list_set lst idx value =
  List.mapi (fun i v -> if i = idx then value else v) lst

let rec map_set m k v =
  match m with
    | [] -> [(k,Obj.repr v)]
    | (k2,v2)::tl -> if k2 = k then (k,Obj.repr v)::tl else (k2,v2)::map_set tl k v

let map_get m k = Obj.obj (List.assoc k m)

let list_union a b = List.sort_uniq compare (a @ b)
let list_except a b = List.filter (fun x -> not (List.mem x b)) a
let list_intersect a b = List.filter (fun x -> List.mem x b) a |> List.sort_uniq compare
let list_union_all a b = a @ b
let sum lst = List.fold_left (+) 0 lst

let products = [[("name",Obj.repr "Laptop");("price",Obj.repr 1500)];[("name",Obj.repr "Smartphone");("price",Obj.repr 900)];[("name",Obj.repr "Tablet");("price",Obj.repr 600)];[("name",Obj.repr "Monitor");("price",Obj.repr 300)];[("name",Obj.repr "Keyboard");("price",Obj.repr 100)];[("name",Obj.repr "Mouse");("price",Obj.repr 50)];[("name",Obj.repr "Headphones");("price",Obj.repr 200)]]
let expensive = (let __res0 = ref [] in
  List.iter (fun p ->
      __res0 := p :: !__res0;
  ) products;
List.rev !__res0)


let () =
  print_endline (__show ("--- Top products (excluding most expensive) ---"));
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | item::rest ->
        try
          print_endline (__show (item.name) ^ " " ^ __show ("costs $") ^ " " ^ __show (item.price));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 expensive with Break -> ()
