open System

type Nation = { n_nationkey:int; n_name:string }
type Customer = { c_custkey:int; c_name:string; c_acctbal:float; c_nationkey:int; c_address:string; c_phone:string; c_comment:string }
type Order = { o_orderkey:int; o_custkey:int; o_orderdate:string }
type LineItem = { l_orderkey:int; l_returnflag:string; l_extendedprice:float; l_discount:float }

let nation = [ { n_nationkey = 1; n_name = "BRAZIL" } ]

let customer = [
    { c_custkey = 1; c_name = "Alice"; c_acctbal = 100.0; c_nationkey = 1; c_address = "123 St"; c_phone = "123-456"; c_comment = "Loyal" }
]

let orders = [
    { o_orderkey = 1000; o_custkey = 1; o_orderdate = "1993-10-15" }
    { o_orderkey = 2000; o_custkey = 1; o_orderdate = "1994-01-02" }
]

let lineitem = [
    { l_orderkey = 1000; l_returnflag = "R"; l_extendedprice = 1000.0; l_discount = 0.1 }
    { l_orderkey = 2000; l_returnflag = "N"; l_extendedprice = 500.0; l_discount = 0.0 }
]

let start_date = "1993-10-01"
let end_date = "1994-01-01"

let joined =
    [ for c in customer do
        for o in orders do
            if o.o_custkey = c.c_custkey then
                for l in lineitem do
                    if l.l_orderkey = o.o_orderkey then
                        for n in nation do
                            if n.n_nationkey = c.c_nationkey &&
                               o.o_orderdate >= start_date &&
                               o.o_orderdate < end_date &&
                               l.l_returnflag = "R" then
                                yield (c, l, n) ]

let grouped =
    joined
    |> List.groupBy (fun (c, _, n) -> (c, n))
    |> List.sortByDescending (fun (_, items) -> items |> List.sumBy (fun (_, l, _) -> l.l_extendedprice * (1.0 - l.l_discount)))
    |> List.map (fun ((c, n), items) ->
        let revenue = items |> List.sumBy (fun (_, l, _) -> l.l_extendedprice * (1.0 - l.l_discount))
        {| c_custkey = c.c_custkey; c_name = c.c_name; revenue = revenue; c_acctbal = c.c_acctbal; n_name = n.n_name; c_address = c.c_address; c_phone = c.c_phone; c_comment = c.c_comment |})

printfn "%A" grouped
