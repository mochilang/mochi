// Generated 2025-07-21 18:37 +0700

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
    mutable order: obj
    mutable customer: obj
}
type Anon6 = {
    mutable order: obj
    mutable customer: obj
}
let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }; { id = 4; name = "Diana" }]
let orders: Anon4 list = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 2; total = 125 }; { id = 102; customerId = 1; total = 300 }; { id = 103; customerId = 5; total = 80 }]
let result: Anon6 list = [ for o in orders do for c in customers do if (o.customerId) = (c.id) then yield { order = o; customer = c } ]
printfn "%s" (string "--- Outer Join using syntax ---")
for row in result do
if row.order then
if row.customer then
printfn "%s" (String.concat " " [string "Order"; string (row.order.id); string "by"; string (row.customer.name); string "- $"; string (row.order.total)])
else
printfn "%s" (String.concat " " [string "Order"; string (row.order.id); string "by"; string "Unknown"; string "- $"; string (row.order.total)])
else
printfn "%s" (String.concat " " [string "Customer"; string (row.customer.name); string "has no orders"])
