open System

type Customer = { id: int; name: string }
type Order = { id: int; customerId: int }

let customers = [
    { id = 1; name = "Alice" }
    { id = 2; name = "Bob" }
]

let orders = [
    { id = 100; customerId = 1 }
    { id = 101; customerId = 1 }
    { id = 102; customerId = 2 }
]

let joined =
    [ for o in orders do
        match customers |> List.tryFind (fun c -> c.id = o.customerId) with
        | Some c -> yield (o, c)
        | None -> () ]

let stats =
    joined
    |> List.groupBy (fun (_, c) -> c.name)
    |> List.map (fun (name, items) -> {| name = name; count = List.length items |})

printfn "--- Orders per customer ---"
for s in stats do
    printfn "%s orders: %d" s.name s.count
