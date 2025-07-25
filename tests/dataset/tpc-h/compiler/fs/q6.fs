// Generated by Mochi compiler v0.10.28 on 2025-07-18T03:37:44Z
open System
open System.Text.Json

type Anon1 = {
    l_extendedprice: float
    l_discount: float
    l_shipdate: string
    l_quantity: int
}
let lineitem: Anon1 list = [{ l_extendedprice = 1000.0; l_discount = 0.06; l_shipdate = "1994-02-15"; l_quantity = 10 }; { l_extendedprice = 500.0; l_discount = 0.07; l_shipdate = "1994-03-10"; l_quantity = 23 }; { l_extendedprice = 400.0; l_discount = 0.04; l_shipdate = "1994-04-10"; l_quantity = 15 }; { l_extendedprice = 200.0; l_discount = 0.06; l_shipdate = "1995-01-01"; l_quantity = 5 }]
let result: obj list = [ for l in lineitem do if (l.l_shipdate >= "1994-01-01") && (l.l_shipdate < "1995-01-01") && (l.l_discount >= 0.05) && (l.l_discount <= 0.07) && (l.l_quantity < 24) then yield List.sum l.l_extendedprice * l.l_discount ]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = ((1000.0 * 0.06) + (500.0 * 0.07)))
