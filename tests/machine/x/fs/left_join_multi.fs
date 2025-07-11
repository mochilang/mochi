open System

type Anon1 = {
    id: int
    name: string
}
type Anon2 = {
    id: int
    customerId: int
}
type Anon3 = {
    orderId: int
    sku: string
}
type Anon4 = {
    orderId: obj
    name: obj
    item: obj
}
let customers: obj list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders: obj list = [{ id = 100; customerId = 1 }; { id = 101; customerId = 2 }]
let items: obj list = [{ orderId = 100; sku = "a" }]
let result: obj list = [ for o in orders do 
  for c in customers do 
  let i = List.tryFind (fun i -> o.id = i.orderId) items if o.customerId = c.id then yield { orderId = o.id; name = c.name; item = i } ]
printfn "%s" "--- Left Join Multi ---"
for r in result do
    printfn "%s" (String.concat " " [string r.orderId; string r.name; string r.item])
