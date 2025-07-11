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
    name: obj
    sku: obj
}
let customers: obj list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders: obj list = [{ id = 100; customerId = 1 }; { id = 101; customerId = 2 }]
let items: obj list = [{ orderId = 100; sku = "a" }; { orderId = 101; sku = "b" }]
let result: obj list = [ for o in orders do 
  for c in customers do 
  for i in items do if o.customerId = c.id && o.id = i.orderId then yield { name = c.name; sku = i.sku } ]
printfn "%s" "--- Multi Join ---"
for r in result do
    printfn "%s" (String.concat " " [string r.name; string "bought item"; string r.sku])
