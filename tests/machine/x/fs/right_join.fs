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
type Anon3 = {
    customerName: obj
    order: obj
}
let customers: obj list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }; { id = 4; name = "Diana" }]
let orders: obj list = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 2; total = 125 }; { id = 102; customerId = 1; total = 300 }]
let result: obj list = [ for o in orders do 
  let c = List.tryFind (fun c -> o.customerId = c.id) customers yield { customerName = c.name; order = o } ]
printfn "%s" "--- Right Join using syntax ---"
for entry in result do
    if entry.order then
        printfn "%s" (String.concat " " [string "Customer"; string entry.customerName; string "has order"; string entry.order.id; string "- $"; string entry.order.total])
    else
        printfn "%s" (String.concat " " [string "Customer"; string entry.customerName; string "has no orders"])
