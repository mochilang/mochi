// Generated by Mochi compiler v0.10.28 on 2025-07-18T03:37:59Z
open System
open System.Text.Json

type Anon1 = {
    p_partkey: int
    p_brand: string
    p_container: string
}
type Anon2 = {
    l_partkey: int
    l_quantity: int
    l_extendedprice: float
}
let part: Anon1 list = [{ p_partkey = 1; p_brand = "Brand#23"; p_container = "MED BOX" }; { p_partkey = 2; p_brand = "Brand#77"; p_container = "LG JAR" }]
let lineitem: Anon2 list = [{ l_partkey = 1; l_quantity = 1; l_extendedprice = 100.0 }; { l_partkey = 1; l_quantity = 10; l_extendedprice = 1000.0 }; { l_partkey = 1; l_quantity = 20; l_extendedprice = 2000.0 }; { l_partkey = 2; l_quantity = 5; l_extendedprice = 500.0 }]
let brand: string = "Brand#23"
let container: string = "MED BOX"
let filtered: float list = [ for l in lineitem do 
  for p in part do if p.p_partkey = l.l_partkey && ((p.p_brand = brand) && (p.p_container = container) && (l.l_quantity < (0.2 * (float (List.sum [ for x in lineitem do if x.l_partkey = p.p_partkey then yield x.l_quantity ]) / float (List.length [ for x in lineitem do if x.l_partkey = p.p_partkey then yield x.l_quantity ]))))) then yield l.l_extendedprice ]
let result = List.sum filtered / 7.0
printfn "%A" (JsonSerializer.Serialize(result))
let expected: float = 100.0 / 7.0
assert (result = expected)
