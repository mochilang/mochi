open System

type Customer = { id:int; name:string }
type Order = { id:int; customerId:int; total:int }

let customers = [
    { id = 1; name = "Alice" }
    { id = 2; name = "Bob" }
    { id = 3; name = "Charlie" }
    { id = 4; name = "Diana" }
]

let orders = [
    { id = 100; customerId = 1; total = 250 }
    { id = 101; customerId = 2; total = 125 }
    { id = 102; customerId = 1; total = 300 }
]

let result =
    [ for c in customers do
        let order = orders |> List.tryFind (fun o -> o.customerId = c.id)
        yield {| customerName = c.name; order = order |} ]

printfn "--- Right Join using syntax ---"
for entry in result do
    match entry.order with
    | Some o ->
        printfn "Customer %s has order %d - $%d" entry.customerName o.id o.total
    | None ->
        printfn "Customer %s has no orders" entry.customerName
