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
    orderId: obj
    customer: obj
    total: obj
}
let customers: obj list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders: obj list = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 3; total = 80 }]
let result: obj list = [ for o in orders do 
  let c = List.tryFind (fun c -> o.customerId = c.id) customers yield { orderId = o.id; customer = c; total = o.total } ]
printfn "%s" "--- Left Join ---"
for entry in result do
    printfn "%s" (String.concat " " [string "Order"; string entry.orderId; string "customer"; string entry.customer; string "total"; string entry.total])
