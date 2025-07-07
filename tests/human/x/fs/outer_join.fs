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
    { id = 103; customerId = 5; total = 80 }
]

let result =
    let orderPart =
        [ for o in orders do
            let customer = customers |> List.tryFind (fun c -> c.id = o.customerId)
            yield {| order = Some o; customer = customer |} ]
    let customerPart =
        [ for c in customers do
            if orders |> List.exists (fun o -> o.customerId = c.id) |> not then
                yield {| order = None; customer = Some c |} ]
    orderPart @ customerPart

printfn "--- Outer Join using syntax ---"
for row in result do
    match row.order, row.customer with
    | Some o, Some c ->
        printfn "Order %d by %s - $%d" o.id c.name o.total
    | Some o, None ->
        printfn "Order %d by Unknown - $%d" o.id o.total
    | None, Some c ->
        printfn "Customer %s has no orders" c.name
    | None, None -> ()
