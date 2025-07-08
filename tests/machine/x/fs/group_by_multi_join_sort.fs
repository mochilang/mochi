open System

exception Break
exception Continue

let nation = [dict [(n_nationkey, 1); (n_name, "BRAZIL")]]
let customer = [dict [(c_custkey, 1); (c_name, "Alice"); (c_acctbal, 100); (c_nationkey, 1); (c_address, "123 St"); (c_phone, "123-456"); (c_comment, "Loyal")]]
let orders = [dict [(o_orderkey, 1000); (o_custkey, 1); (o_orderdate, "1993-10-15")]; dict [(o_orderkey, 2000); (o_custkey, 1); (o_orderdate, "1994-01-02")]]
let lineitem = [dict [(l_orderkey, 1000); (l_returnflag, "R"); (l_extendedprice, 1000); (l_discount, 0.1)]; dict [(l_orderkey, 2000); (l_returnflag, "N"); (l_extendedprice, 500); (l_discount, 0)]]
let start_date = "1993-10-01"
let end_date = "1994-01-01"
let result = [ for c in customer do if o.o_orderdate >= start_date && o.o_orderdate < end_date && l.l_returnflag = "R" then yield dict [(c_custkey, g.key.c_custkey); (c_name, g.key.c_name); (revenue, sum [ for x in g doyield x.l.l_extendedprice * (1 - x.l.l_discount) ]); (c_acctbal, g.key.c_acctbal); (n_name, g.key.n_name); (c_address, g.key.c_address); (c_phone, g.key.c_phone); (c_comment, g.key.c_comment)] ] |> List.sortByDescending (fun _ -> sum [ for x in g doyield x.l.l_extendedprice * (1 - x.l.l_discount) ])
printfn "%A" (result)
