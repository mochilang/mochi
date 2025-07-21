// Generated 2025-07-21 18:37 +0700

type Anon1 = {
    mutable id: int
    mutable name: string
}
type Anon2 = {
    mutable id: int
    mutable name: string
}
type Anon3 = {
    mutable id: int
    mutable nation: int
}
type Anon4 = {
    mutable id: int
    mutable nation: int
}
type Anon5 = {
    mutable part: int
    mutable supplier: int
    mutable cost: float
    mutable qty: int
}
type Anon6 = {
    mutable part: int
    mutable supplier: int
    mutable cost: float
    mutable qty: int
}
type Anon7 = {
    mutable part: obj
    mutable value: obj
}
type Anon8 = {
    mutable part: obj
    mutable value: obj
}
type Anon9 = {
    mutable part: obj
    mutable total: float
}
type Anon10 = {
    mutable x: obj
}
type Anon11 = {
    mutable key: obj
    mutable items: Anon10 list
}
type Anon12 = {
    mutable part: obj
    mutable total: float
}
let nations: Anon2 list = [{ id = 1; name = "A" }; { id = 2; name = "B" }]
let suppliers: Anon4 list = [{ id = 1; nation = 1 }; { id = 2; nation = 2 }]
let partsupp: Anon6 list = [{ part = 100; supplier = 1; cost = 10.0; qty = 2 }; { part = 100; supplier = 2; cost = 20.0; qty = 1 }; { part = 200; supplier = 1; cost = 5.0; qty = 3 }]
let filtered: Anon8 list = [ for ps in partsupp do for s in suppliers do if (s.id) = (ps.supplier) then for n in nations do if (n.id) = (s.nation) then if (n.name) = "A" then yield { part = ps.part; value = (ps.cost) * (ps.qty) } ]
let grouped: Anon12 list = [ for (key, items) in List.groupBy (fun x -> x.part) filtered do
    let g : Anon11 = { key = key; items = items }
    yield { part = g.key; total = List.sum [ for r in g.items do yield r.value ] } ]
printfn "%s" (("[" + (String.concat ", " (List.map string grouped))) + "]")
