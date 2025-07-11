open System

type Anon1 = {
    id: int
    name: string
}
type Anon2 = {
    id: int
    customerId: int
    total: int
}
let customers: obj list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }; { id = 4; name = "Diana" }]
let orders: obj list = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 2; total = 125 }; { id = 102; customerId = 1; total = 300 }; { id = 103; customerId = 5; total = 80 }]
let result: obj list = (let orderPart = [ for o in orders do
    let c = customers |> List.tryFind (fun c -> o.customerId = c.id)
    yield { order = Some o; customer = c } ]
 let customerPart = [ for c in customers do
    if orders |> List.exists (fun o -> o.customerId = c.id) |> not then
        yield { order = None; customer = Some c } ]
 orderPart @ customerPart)
printfn "%s" "--- Outer Join using syntax ---"
for row in result do
    if row.order then
        if row.customer then
            printfn "%s" (String.concat " " [string "Order"; string row.order.id; string "by"; string row.customer.name; string "- $"; string row.order.total])
        else
            printfn "%s" (String.concat " " [string "Order"; string row.order.id; string "by"; string "Unknown"; string "- $"; string row.order.total])
    else
        printfn "%s" (String.concat " " [string "Customer"; string row.customer.name; string "has no orders"])
