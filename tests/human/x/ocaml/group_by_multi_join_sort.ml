(* Hand-written OCaml version of group_by_multi_join_sort.mochi *)

type nation = { n_nationkey:int; n_name:string }
let nation = [ { n_nationkey=1; n_name="BRAZIL" } ]

type customer = {
  c_custkey:int; c_name:string; c_acctbal:float; c_nationkey:int;
  c_address:string; c_phone:string; c_comment:string }
let customer = [
  { c_custkey=1; c_name="Alice"; c_acctbal=100.0; c_nationkey=1;
    c_address="123 St"; c_phone="123-456"; c_comment="Loyal" }
]

type order = { o_orderkey:int; o_custkey:int; o_orderdate:string }
let orders = [
  { o_orderkey=1000; o_custkey=1; o_orderdate="1993-10-15" };
  { o_orderkey=2000; o_custkey=1; o_orderdate="1994-01-02" };
]

type lineitem = {
  l_orderkey:int; l_returnflag:string; l_extendedprice:float; l_discount:float }
let lineitem = [
  { l_orderkey=1000; l_returnflag="R"; l_extendedprice=1000.0; l_discount=0.1 };
  { l_orderkey=2000; l_returnflag="N"; l_extendedprice=500.0; l_discount=0.0 };
]

let start_date = "1993-10-01"
let end_date   = "1994-01-01"

(* collect all joined rows that satisfy filters *)
type row = { c:customer; o:order; l:lineitem; n:nation }

let rows =
  List.fold_left (fun acc c ->
    let os = List.filter (fun o -> o.o_custkey = c.c_custkey) orders in
    List.fold_left (fun acc o ->
      let ls = List.filter (fun l -> l.l_orderkey = o.o_orderkey) lineitem in
      List.fold_left (fun acc l ->
        let ns = List.filter (fun n -> n.n_nationkey = c.c_nationkey) nation in
        List.fold_left (fun acc n ->
          if o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag = "R" then
            {c;c_address=c.c_address;o;l;n} :: acc
          else acc
        ) acc ns
      ) acc ls
    ) acc os
  ) [] customer
  |> List.rev

(* group by customer/nation info *)
let add_group map row =
  let key = (row.c.c_custkey, row.c.c_name, row.c.c_acctbal,
             row.c.c_address, row.c.c_phone, row.c.c_comment,
             row.n.n_name) in
  let lst = try List.assoc key map with Not_found -> [] in
  (key, row :: lst) :: List.remove_assoc key map

let groups = List.fold_left add_group [] rows

(* compute revenue and build result records *)
type result = {
  c_custkey:int; c_name:string; revenue:float; c_acctbal:float;
  n_name:string; c_address:string; c_phone:string; c_comment:string }

let results =
  groups
  |> List.map (fun (key, lst) ->
       let revenue =
         List.fold_left (fun s r -> s +.
           r.l.l_extendedprice *. (1. -. r.l.l_discount)) 0.0 lst in
       let (ck,cn,cb,ca,cp,cc,nn) = key in
       { c_custkey=ck; c_name=cn; revenue; c_acctbal=cb;
         n_name=nn; c_address=ca; c_phone=cp; c_comment=cc })
  |> List.sort (fun a b -> compare b.revenue a.revenue)

let () =
  List.iter (fun r ->
    Printf.printf "{c_custkey=%d; c_name=%s; revenue=%g; c_acctbal=%g; n_name=%s; c_address=%s; c_phone=%s; c_comment=%s}\n"
      r.c_custkey r.c_name r.revenue r.c_acctbal r.n_name r.c_address r.c_phone r.c_comment
  ) results
