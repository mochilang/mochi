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

let nation = [[("n_nationkey",1);("n_name","BRAZIL")]]
let customer = [[("c_custkey",1);("c_name","Alice");("c_acctbal",100);("c_nationkey",1);("c_address","123 St");("c_phone","123-456");("c_comment","Loyal")]]
let orders = [[("o_orderkey",1000);("o_custkey",1);("o_orderdate","1993-10-15")];[("o_orderkey",2000);("o_custkey",1);("o_orderdate","1994-01-02")]]
let lineitem = [[("l_orderkey",1000);("l_returnflag","R");("l_extendedprice",1000);("l_discount",0.1)];[("l_orderkey",2000);("l_returnflag","N");("l_extendedprice",500);("l_discount",0)]]
let start_date = "1993-10-01"
let end_date = "1994-01-01"
let result = (let __res0 = ref [] in
  List.iter (fun c ->
      if (((((o.o_orderdate >= start_date) && o.o_orderdate) < end_date) && l.l_returnflag) = "R") then
    __res0 := [("c_custkey",g.key.c_custkey);("c_name",g.key.c_name);("revenue",sum (let __res1 = ref [] in
  List.iter (fun x ->
      __res1 := (x.l.l_extendedprice * ((1 - x.l.l_discount))) :: !__res1;
  ) g;
List.rev !__res1)
);("c_acctbal",g.key.c_acctbal);("n_name",g.key.n_name);("c_address",g.key.c_address);("c_phone",g.key.c_phone);("c_comment",g.key.c_comment)] :: !__res0;
  ) customer;
List.rev !__res0)


let () =
  print_endline (__show (result));
