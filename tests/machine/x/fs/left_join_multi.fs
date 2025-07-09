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
    orderId: obj
    name: obj
    item: obj
}
let customers = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders = [{ id = 100; customerId = 1 }; { id = 101; customerId = 2 }]
let items = [{ orderId = 100; sku = "a" }]
let result = [ for o in orders do 
  for c in customers do 
  let i = List.tryFind (fun i -> o.id = i.orderId) items if o.customerId = c.id then yield { orderId = o.id; name = c.name; item = i } ]
printfn "%s" "--- Left Join Multi ---"
try
    for r in result do
        try
            printfn "%s" (String.concat " " [string r.orderId; string r.name; string r.item])
        with Continue -> ()
with Break -> ()
