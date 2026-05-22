// Generated 2025-07-22 04:52 +0700

type Anon1 = {
    n_nationkey: int
    n_name: string
}
type Anon2 = {
    n_nationkey: int
    n_name: string
}
type Anon3 = {
    c_custkey: int
    c_name: string
    c_acctbal: float
    c_nationkey: int
    c_address: string
    c_phone: string
    c_comment: string
}
type Anon4 = {
    c_custkey: int
    c_name: string
    c_acctbal: float
    c_nationkey: int
    c_address: string
    c_phone: string
    c_comment: string
}
type Anon5 = {
    o_orderkey: int
    o_custkey: int
    o_orderdate: string
}
type Anon6 = {
    o_orderkey: int
    o_custkey: int
    o_orderdate: string
}
type Anon7 = {
    l_orderkey: int
    l_returnflag: string
    l_extendedprice: float
    l_discount: float
}
type Anon8 = {
    l_orderkey: int
    l_returnflag: string
    l_extendedprice: float
    l_discount: float
}
type Anon9 = {
    c_custkey: obj
    c_name: obj
    c_acctbal: obj
    c_address: obj
    c_phone: obj
    c_comment: obj
    n_name: obj
}
type Anon10 = {
    c_custkey: obj
    c_name: obj
    revenue: float
    c_acctbal: obj
    n_name: obj
    c_address: obj
    c_phone: obj
    c_comment: obj
}
type Anon11 = {
    c: Anon4
    o: Anon6
    l: Anon8
    n: Anon2
}
type Anon12 = {
    key: Anon9
    items: Anon11 list
}
type Anon13 = {
    c_custkey: obj
    c_name: obj
    revenue: float
    c_acctbal: obj
    n_name: obj
    c_address: obj
    c_phone: obj
    c_comment: obj
}
let nation: Anon2 list = [{ n_nationkey = 1; n_name = "BRAZIL" }]
let customer: Anon4 list = [{ c_custkey = 1; c_name = "Alice"; c_acctbal = 100.0; c_nationkey = 1; c_address = "123 St"; c_phone = "123-456"; c_comment = "Loyal" }]
let orders: Anon6 list = [{ o_orderkey = 1000; o_custkey = 1; o_orderdate = "1993-10-15" }; { o_orderkey = 2000; o_custkey = 1; o_orderdate = "1994-01-02" }]
let lineitem: Anon8 list = [{ l_orderkey = 1000; l_returnflag = "R"; l_extendedprice = 1000.0; l_discount = 0.1 }; { l_orderkey = 2000; l_returnflag = "N"; l_extendedprice = 500.0; l_discount = 0.0 }]
let start_date: string = "1993-10-01"
let end_date: string = "1994-01-01"
let result: Anon13 list = [ for (key, items) in List.groupBy (fun { c = c; o = o; l = l; n = n } -> { c_custkey = c.c_custkey; c_name = c.c_name; c_acctbal = c.c_acctbal; c_address = c.c_address; c_phone = c.c_phone; c_comment = c.c_comment; n_name = n.n_name }) [ for c in customer do for o in orders do if (o.o_custkey) = (c.c_custkey) then for l in lineitem do if (l.l_orderkey) = (o.o_orderkey) then for n in nation do if (n.n_nationkey) = (c.c_nationkey) then if (((o.o_orderdate) >= start_date) && ((o.o_orderdate) < end_date)) && ((l.l_returnflag) = "R") then yield { c = c; o = o; l = l; n = n } : Anon11 ] do
    let g : Anon12 = { key = key; items = items }
    yield { c_custkey = g.key.c_custkey; c_name = g.key.c_name; revenue = List.sum [ for x in g.items do yield (x.l.l_extendedprice) * (1 - (x.l.l_discount)) ]; c_acctbal = g.key.c_acctbal; n_name = g.key.n_name; c_address = g.key.c_address; c_phone = g.key.c_phone; c_comment = g.key.c_comment } ]
printfn "%s" (("[" + (String.concat ", " (List.map string result))) + "]")
