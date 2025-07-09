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
}
type Anon3 = {
    orderId: int
    sku: string
}
type Anon4 = {
    name: obj
    sku: obj
}
let customers = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders = [{ id = 100; customerId = 1 }; { id = 101; customerId = 2 }]
let items = [{ orderId = 100; sku = "a" }; { orderId = 101; sku = "b" }]
let result = [ for o in orders do 
  for c in customers do 
  for i in items do if o.customerId = c.id && o.id = i.orderId then yield { name = c.name; sku = i.sku } ]
printfn "%s" "--- Multi Join ---"
try
    for r in result do
        try
            printfn "%s" (String.concat " " [string r.name; string "bought item"; string r.sku])
        with Continue -> ()
with Break -> ()
