// Generated by Mochi compiler v0.10.28 on 2025-07-18T03:37:58Z
open System
open System.Text.Json

type Anon1 = {
    s_suppkey: int
    s_name: string
    s_address: string
    s_comment: string
}
type Anon2 = {
    p_partkey: int
    p_brand: string
    p_type: string
    p_size: int
}
type Anon3 = {
    ps_partkey: int
    ps_suppkey: int
}
type Anon4 = {
    s_name: obj
    s_address: obj
}
type Anon5 = {
    s_name: string
    s_address: string
}
let supplier: Anon1 list = [{ s_suppkey = 100; s_name = "AlphaSupply"; s_address = "123 Hilltop"; s_comment = "Reliable and efficient" }; { s_suppkey = 200; s_name = "BetaSupply"; s_address = "456 Riverside"; s_comment = "Known for Customer Complaints" }]
let part: Anon2 list = [{ p_partkey = 1; p_brand = "Brand#12"; p_type = "SMALL ANODIZED"; p_size = 5 }; { p_partkey = 2; p_brand = "Brand#23"; p_type = "MEDIUM POLISHED"; p_size = 10 }]
let partsupp: Anon3 list = [{ ps_partkey = 1; ps_suppkey = 100 }; { ps_partkey = 2; ps_suppkey = 200 }]
let excluded_suppliers: int list = [ for ps in partsupp do 
  for p in part do if p.p_partkey = ps.ps_partkey && p.p_brand = "Brand#12" && p.p_type.contains("SMALL") && p.p_size = 5 then yield ps.ps_suppkey ]
let result: Anon5 list = [ for s in supplier do if not (List.contains s.s_suppkey excluded_suppliers) && (not s.s_comment.contains("Customer")) && (not s.s_comment.contains("Complaints")) then yield (s.s_name, { s_name = s.s_name; s_address = s.s_address }) ] |> List.sortBy fst |> List.map snd
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [])
