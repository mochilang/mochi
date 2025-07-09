open System

exception Break
exception Continue

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
    order: obj
    customer: obj
}
let customers = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }; { id = 4; name = "Diana" }]
let orders = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 2; total = 125 }; { id = 102; customerId = 1; total = 300 }; { id = 103; customerId = 5; total = 80 }]
let result = [ for o in orders do 
  for c in customers do if o.customerId = c.id then yield { order = o; customer = c } ]
printfn "%s" "--- Outer Join using syntax ---"
try
    for row in result do
        try
            if row.order then
                if row.customer then
                    printfn "%s" (String.concat " " [string "Order"; string row.order.id; string "by"; string row.customer.name; string "- $"; string row.order.total])
                else
                    printfn "%s" (String.concat " " [string "Order"; string row.order.id; string "by"; string "Unknown"; string "- $"; string row.order.total])
            else
                printfn "%s" (String.concat " " [string "Customer"; string row.customer.name; string "has no orders"])
        with Continue -> ()
with Break -> ()
