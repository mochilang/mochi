// Generated 2025-07-22 04:52 +0700

type Anon1 = {
    id: int
    name: string
}
type Anon2 = {
    id: int
    name: string
}
type Anon3 = {
    id: int
    nation: int
}
type Anon4 = {
    id: int
    nation: int
}
type Anon5 = {
    part: int
    supplier: int
    cost: float
    qty: int
}
type Anon6 = {
    part: int
    supplier: int
    cost: float
    qty: int
}
type Anon7 = {
    part: obj
    value: obj
}
type Anon8 = {
    part: obj
    value: obj
}
type Anon9 = {
    part: obj
    total: float
}
type Anon10 = {
    x: Anon8
}
type Anon11 = {
    key: obj
    items: Anon10 list
}
type Anon12 = {
    part: obj
    total: float
}
let nations: Anon2 list = [{ id = 1; name = "A" }; { id = 2; name = "B" }]
let suppliers: Anon4 list = [{ id = 1; nation = 1 }; { id = 2; nation = 2 }]
let partsupp: Anon6 list = [{ part = 100; supplier = 1; cost = 10.0; qty = 2 }; { part = 100; supplier = 2; cost = 20.0; qty = 1 }; { part = 200; supplier = 1; cost = 5.0; qty = 3 }]
let filtered: Anon8 list = [ for ps in partsupp do for s in suppliers do if (s.id) = (ps.supplier) then for n in nations do if (n.id) = (s.nation) then if (n.name) = "A" then yield { part = ps.part; value = (ps.cost) * (ps.qty) } ]
let grouped: Anon12 list = [ for (key, items) in List.groupBy (fun x -> x.part) filtered do
    let g : Anon11 = { key = key; items = items }
    yield { part = g.key; total = List.sum [ for r in g.items do yield r.value ] } ]
printfn "%s" (("[" + (String.concat ", " (List.map string grouped))) + "]")
