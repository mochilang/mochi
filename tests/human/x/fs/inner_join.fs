open System

type Customer = { id: int; name: string }
type Order = { id: int; customerId: int; total: int }

let customers = [
    { id = 1; name = "Alice" }
    { id = 2; name = "Bob" }
    { id = 3; name = "Charlie" }
]

let orders = [
    { id = 100; customerId = 1; total = 250 }
    { id = 101; customerId = 2; total = 125 }
    { id = 102; customerId = 1; total = 300 }
    { id = 103; customerId = 4; total = 80 }
]

let result =
    [ for o in orders do
        match customers |> List.tryFind (fun c -> c.id = o.customerId) with
        | Some c -> yield {| orderId = o.id; customerName = c.name; total = o.total |}
        | None -> () ]

printfn "--- Orders with customer info ---"
for e in result do
    printfn "Order %d by %s - $%d" e.orderId e.customerName e.total
