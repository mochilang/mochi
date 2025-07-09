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

let nations = [[("id",1);("name","A")];[("id",2);("name","B")]]
let suppliers = [[("id",1);("nation",1)];[("id",2);("nation",2)]]
let partsupp = [[("part",100);("supplier",1);("cost",10);("qty",2)];[("part",100);("supplier",2);("cost",20);("qty",1)];[("part",200);("supplier",1);("cost",5);("qty",3)]]
let filtered = (let __res0 = ref [] in
  List.iter (fun ps ->
      if (n.name = "A") then
    __res0 := [("part",ps.part);("value",(ps.cost * ps.qty))] :: !__res0;
  ) partsupp;
List.rev !__res0)

let grouped = (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := [("part",g.key);("total",sum (let __res2 = ref [] in
  List.iter (fun r ->
      __res2 := r.value :: !__res2;
  ) g;
List.rev !__res2)
)] :: !__res1;
  ) filtered;
List.rev !__res1)


let () =
  print_endline (__show (grouped));
