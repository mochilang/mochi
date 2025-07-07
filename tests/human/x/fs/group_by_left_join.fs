open System

type Customer = { id: int; name: string }
type Order = { id: int; customerId: int }

let customers = [
    { id = 1; name = "Alice" }
    { id = 2; name = "Bob" }
    { id = 3; name = "Charlie" }
]

let orders = [
    { id = 100; customerId = 1 }
    { id = 101; customerId = 1 }
    { id = 102; customerId = 2 }
]

let joined =
    [ for c in customers do
        let os = orders |> List.filter (fun o -> o.customerId = c.id)
        if List.isEmpty os then
            yield (c, None)
        else
            for o in os do
                yield (c, Some o) ]

let stats =
    joined
    |> List.groupBy (fun (c, _) -> c.name)
    |> List.map (fun (name, items) ->
        let count = items |> List.choose snd |> List.length
        {| name = name; count = count |})

printfn "--- Group Left Join ---"
for s in stats do
    printfn "%s orders: %d" s.name s.count
