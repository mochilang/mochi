open System
open System.Text.Json

type Anon1 = {
    r_regionkey: int
    r_name: string
}
type Anon2 = {
    n_nationkey: int
    n_regionkey: int
    n_name: string
}
type Anon3 = {
    s_suppkey: int
    s_name: string
    s_address: string
    s_nationkey: int
    s_phone: string
    s_acctbal: float
    s_comment: string
}
type Anon4 = {
    p_partkey: int
    p_type: string
    p_size: int
    p_mfgr: string
}
type Anon5 = {
    ps_partkey: int
    ps_suppkey: int
    ps_supplycost: float
}
type Anon6 = {
    s: obj
    n: obj
}
type Anon7 = {
    s_acctbal: obj
    s_name: obj
    n_name: obj
    p_partkey: obj
    p_mfgr: obj
    s_address: obj
    s_phone: obj
    s_comment: obj
    ps_supplycost: obj
}
type Anon8 = {
    s_acctbal: float
    s_name: string
    n_name: string
    p_partkey: int
    p_mfgr: string
    s_address: string
    s_phone: string
    s_comment: string
    ps_supplycost: float
}
let region: obj list = [{ r_regionkey = 1; r_name = "EUROPE" }; { r_regionkey = 2; r_name = "ASIA" }]
let nation: obj list = [{ n_nationkey = 10; n_regionkey = 1; n_name = "FRANCE" }; { n_nationkey = 20; n_regionkey = 2; n_name = "CHINA" }]
let supplier: obj list = [{ s_suppkey = 100; s_name = "BestSupplier"; s_address = "123 Rue"; s_nationkey = 10; s_phone = "123"; s_acctbal = 1000; s_comment = "Fast and reliable" }; { s_suppkey = 200; s_name = "AltSupplier"; s_address = "456 Way"; s_nationkey = 20; s_phone = "456"; s_acctbal = 500; s_comment = "Slow" }]
let part: obj list = [{ p_partkey = 1000; p_type = "LARGE BRASS"; p_size = 15; p_mfgr = "M1" }; { p_partkey = 2000; p_type = "SMALL COPPER"; p_size = 15; p_mfgr = "M2" }]
let partsupp: obj list = [{ ps_partkey = 1000; ps_suppkey = 100; ps_supplycost = 10 }; { ps_partkey = 1000; ps_suppkey = 200; ps_supplycost = 15 }]
let europe_nations: obj list = [ for r in region do 
  for n in nation do if n.n_regionkey = r.r_regionkey && r.r_name = "EUROPE" then yield n ]
let europe_suppliers: obj list = [ for s in supplier do 
  for n in europe_nations do if s.s_nationkey = n.n_nationkey then yield { s = s; n = n } ]
let target_parts: obj list = [ for p in part do if p.p_size = 15 && p.p_type = "LARGE BRASS" then yield p ]
let target_partsupp: obj list = [ for ps in partsupp do 
  for p in target_parts do 
  for s in europe_suppliers do if ps.ps_partkey = p.p_partkey && ps.ps_suppkey = s.s.s_suppkey then yield { s_acctbal = s.s.s_acctbal; s_name = s.s.s_name; n_name = s.n.n_name; p_partkey = p.p_partkey; p_mfgr = p.p_mfgr; s_address = s.s.s_address; s_phone = s.s.s_phone; s_comment = s.s.s_comment; ps_supplycost = ps.ps_supplycost } ]
let costs: obj list = [ for x in target_partsupp do yield x.ps_supplycost ]
let min_cost: obj = List.min costs
let result: obj list = [ for x in target_partsupp do if x.ps_supplycost = min_cost then yield x ] |> List.sortByDescending (fun x -> x.s_acctbal)
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ s_acctbal = 1000; s_name = "BestSupplier"; n_name = "FRANCE"; p_partkey = 1000; p_mfgr = "M1"; s_address = "123 Rue"; s_phone = "123"; s_comment = "Fast and reliable"; ps_supplycost = 10 }])
