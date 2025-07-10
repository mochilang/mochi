open System

type Anon1 = {
    id: int
    name: string
}
type Anon2 = {
    id: int
    nation: int
}
type Anon3 = {
    part: int
    supplier: int
    cost: float
    qty: int
}
type Anon4 = {
    part: obj
    value: obj
}
type Anon5 = {
    part: obj
    total: obj
}
let nations = [{ id = 1; name = "A" }; { id = 2; name = "B" }]
let suppliers = [{ id = 1; nation = 1 }; { id = 2; nation = 2 }]
let partsupp = [{ part = 100; supplier = 1; cost = 10; qty = 2 }; { part = 100; supplier = 2; cost = 20; qty = 1 }; { part = 200; supplier = 1; cost = 5; qty = 3 }]
let filtered = [ for ps in partsupp do 
  for s in suppliers do 
  for n in nations do if s.id = ps.supplier && n.id = s.nation && n.name = "A" then yield { part = ps.part; value = ps.cost * ps.qty } ]
let grouped = [ for gKey, gItems in [ for x in filtered do yield x ] |> List.groupBy (fun x -> x.part) do
    let g = {| key = gKey; items = gItems |}
    yield { part = g.key; total = List.sum [ for r in g do yield r.value ] } ]
printfn "%A" (grouped)
