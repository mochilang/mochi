// Generated 2025-07-21 15:37 +0700
open System

type Anon1 = {
    mutable id: int
    mutable name: string
}
type Anon2 = {
    mutable id: int
    mutable name: string
}
type Anon3 = {
    mutable id: int
    mutable customerId: int
    mutable total: int
}
type Anon4 = {
    mutable id: int
    mutable customerId: int
    mutable total: int
}
type Anon5 = {
    mutable orderId: obj
    mutable customerName: obj
    mutable total: obj
}
type Anon6 = {
    mutable orderId: obj
    mutable customerName: obj
    mutable total: obj
}
let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }]
let orders: Anon4 list = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 2; total = 125 }; { id = 102; customerId = 1; total = 300 }; { id = 103; customerId = 4; total = 80 }]
let result: Anon6 list = [ for o in orders do for c in customers do if (o.customerId) = (c.id) then yield { orderId = o.id; customerName = c.name; total = o.total } ]
printfn "%s" (string "--- Orders with customer info ---")
for entry in result do
printfn "%s" (String.concat " " [string "Order"; string (entry.orderId); string "by"; string (entry.customerName); string "- $"; string (entry.total)])
