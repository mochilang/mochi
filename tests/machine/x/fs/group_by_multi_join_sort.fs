open System

type Anon1 = {
    n_nationkey: int
    n_name: string
}
type Anon2 = {
    c_custkey: int
    c_name: string
    c_acctbal: float
    c_nationkey: int
    c_address: string
    c_phone: string
    c_comment: string
}
type Anon3 = {
    o_orderkey: int
    o_custkey: int
    o_orderdate: string
}
type Anon4 = {
    l_orderkey: int
    l_returnflag: string
    l_extendedprice: float
    l_discount: float
}
type Anon5 = {
    c_custkey: obj
    c_name: obj
    revenue: obj
    c_acctbal: obj
    n_name: obj
    c_address: obj
    c_phone: obj
    c_comment: obj
}
type Anon6 = {
    c_custkey: obj
    c_name: obj
    c_acctbal: obj
    c_address: obj
    c_phone: obj
    c_comment: obj
    n_name: obj
}
let nation: obj list = [{ n_nationkey = 1; n_name = "BRAZIL" }]
let customer: obj list = [{ c_custkey = 1; c_name = "Alice"; c_acctbal = 100; c_nationkey = 1; c_address = "123 St"; c_phone = "123-456"; c_comment = "Loyal" }]
let orders: obj list = [{ o_orderkey = 1000; o_custkey = 1; o_orderdate = "1993-10-15" }; { o_orderkey = 2000; o_custkey = 1; o_orderdate = "1994-01-02" }]
let lineitem: obj list = [{ l_orderkey = 1000; l_returnflag = "R"; l_extendedprice = 1000; l_discount = 0.1 }; { l_orderkey = 2000; l_returnflag = "N"; l_extendedprice = 500; l_discount = 0 }]
let start_date: string = "1993-10-01"
let end_date: string = "1994-01-01"
let result: obj list = [ for gKey, gItems in [ for c in customer do 
  for o in orders do 
  for l in lineitem do 
  for n in nation do if o.o_custkey = c.c_custkey && l.l_orderkey = o.o_orderkey && n.n_nationkey = c.c_nationkey && o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag = "R" then yield (c, o, l, n) ] |> List.groupBy (fun (c, o, l, n) -> { c_custkey = c.c_custkey; c_name = c.c_name; c_acctbal = c.c_acctbal; c_address = c.c_address; c_phone = c.c_phone; c_comment = c.c_comment; n_name = n.n_name }) |> List.sortByDescending (fun (gKey, gItems) -> let g = {| key = gKey; items = gItems |} in List.sum [ for x in g do yield x.l.l_extendedprice * (1 - x.l.l_discount) ]) do
    let g = {| key = gKey; items = gItems |}
    yield { c_custkey = g.key.c_custkey; c_name = g.key.c_name; revenue = List.sum [ for x in g do yield x.l.l_extendedprice * (1 - x.l.l_discount) ]; c_acctbal = g.key.c_acctbal; n_name = g.key.n_name; c_address = g.key.c_address; c_phone = g.key.c_phone; c_comment = g.key.c_comment } ]
printfn "%A" (result)
