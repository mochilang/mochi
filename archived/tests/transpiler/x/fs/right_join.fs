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
    mutable customerName: obj
    mutable order: obj
}
type Anon6 = {
    mutable customerName: obj
    mutable order: obj
}
let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }; { id = 4; name = "Diana" }]
let orders: Anon4 list = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 2; total = 125 }; { id = 102; customerId = 1; total = 300 }]
let result: Anon6 list = [ for c in customers do for o in orders do if (o.customerId) = (c.id) then yield { customerName = c.name; order = o } ]
printfn "%s" (string "--- Right Join using syntax ---")
for entry in result do
if entry.order then
printfn "%s" (String.concat " " [string "Customer"; string (entry.customerName); string "has order"; string (entry.order.id); string "- $"; string (entry.order.total)])
else
printfn "%s" (String.concat " " [string "Customer"; string (entry.customerName); string "has no orders"])
