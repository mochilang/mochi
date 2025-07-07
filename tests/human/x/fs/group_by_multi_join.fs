open System

type Nation = { id: int; name: string }
type Supplier = { id: int; nation: int }
type PartSupp = { part: int; supplier: int; cost: float; qty: int }

let nations = [ { id = 1; name = "A" }; { id = 2; name = "B" } ]
let suppliers = [ { id = 1; nation = 1 }; { id = 2; nation = 2 } ]
let partsupp = [
    { part = 100; supplier = 1; cost = 10.0; qty = 2 }
    { part = 100; supplier = 2; cost = 20.0; qty = 1 }
    { part = 200; supplier = 1; cost = 5.0; qty = 3 }
]

let filtered =
    [ for ps in partsupp do
        for s in suppliers do
            if s.id = ps.supplier then
                for n in nations do
                    if n.id = s.nation && n.name = "A" then
                        yield {| part = ps.part; value = ps.cost * float ps.qty |} ]

let grouped =
    filtered
    |> List.groupBy (fun x -> x.part)
    |> List.map (fun (part, items) ->
        let total = items |> List.sumBy (fun r -> r.value)
        {| part = part; total = total |})

printfn "%A" grouped
