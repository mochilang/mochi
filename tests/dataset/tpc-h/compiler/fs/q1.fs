open System
open System.Text.Json

type Anon1 = {
    l_quantity: int
    l_extendedprice: float
    l_discount: float
    l_tax: float
    l_returnflag: string
    l_linestatus: string
    l_shipdate: string
}
type Anon2 = {
    returnflag: obj
    linestatus: obj
    sum_qty: obj
    sum_base_price: obj
    sum_disc_price: obj
    sum_charge: obj
    avg_qty: obj
    avg_price: obj
    avg_disc: obj
    count_order: obj
}
type Anon3 = {
    returnflag: obj
    linestatus: obj
}
type Anon4 = {
    returnflag: string
    linestatus: string
    sum_qty: int
    sum_base_price: int
    sum_disc_price: float
    sum_charge: obj
    avg_qty: float
    avg_price: int
    avg_disc: float
    count_order: int
}
type _Group<'K,'T>(key: 'K) =
    member val key = key with get, set
    member val Items = System.Collections.Generic.List<'T>() with get
    member this.size = this.Items.Count

let _group_by (src: 'T list) (keyfn: 'T -> 'K) : _Group<'K,'T> list =
    let groups = System.Collections.Generic.Dictionary<string,_Group<'K,'T>>()
    let order = System.Collections.Generic.List<string>()
    for it in src do
        let key = keyfn it
        let ks = string key
        let mutable g = Unchecked.defaultof<_Group<'K,'T>>
        if groups.TryGetValue(ks, &g) then ()
        else
            g <- _Group<'K,'T>(key)
            groups.Add(ks, g)
            order.Add(ks)
        g.Items.Add(it)
    [ for ks in order -> groups[ks] ]

let lineitem: obj list = [{ l_quantity = 17; l_extendedprice = 1000; l_discount = 0.05; l_tax = 0.07; l_returnflag = "N"; l_linestatus = "O"; l_shipdate = "1998-08-01" }; { l_quantity = 36; l_extendedprice = 2000; l_discount = 0.1; l_tax = 0.05; l_returnflag = "N"; l_linestatus = "O"; l_shipdate = "1998-09-01" }; { l_quantity = 25; l_extendedprice = 1500; l_discount = 0; l_tax = 0.08; l_returnflag = "R"; l_linestatus = "F"; l_shipdate = "1998-09-03" }]
let result: obj list = [ for g in _group_by [ for row in lineitem do if row.l_shipdate <= "1998-09-02" then yield row ] (fun row -> { returnflag = row.l_returnflag; linestatus = row.l_linestatus }) do
    yield { returnflag = g.key.returnflag; linestatus = g.key.linestatus; sum_qty = List.sum [ for x in g do yield x.l_quantity ]; sum_base_price = List.sum [ for x in g do yield x.l_extendedprice ]; sum_disc_price = List.sum [ for x in g do yield x.l_extendedprice * (1 - x.l_discount) ]; sum_charge = List.sum [ for x in g do yield x.l_extendedprice * (1 - x.l_discount) * (1 + x.l_tax) ]; avg_qty = (List.sum [ for x in g do yield x.l_quantity ] / List.length [ for x in g do yield x.l_quantity ]); avg_price = (List.sum [ for x in g do yield x.l_extendedprice ] / List.length [ for x in g do yield x.l_extendedprice ]); avg_disc = (List.sum [ for x in g do yield x.l_discount ] / List.length [ for x in g do yield x.l_discount ]); count_order = List.length g.Items } ]
printfn "%A" (JsonSerializer.Serialize(result))
assert (result = [{ returnflag = "N"; linestatus = "O"; sum_qty = 53; sum_base_price = 3000; sum_disc_price = 950 + 1800; sum_charge = (950 * 1.07) + (1800 * 1.05); avg_qty = 26.5; avg_price = 1500; avg_disc = 0.07500000000000001; count_order = 2 }])
