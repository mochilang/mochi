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
    mutable orderId: obj
    mutable customer: obj
    mutable total: obj
}
type Anon6 = {
    mutable orderId: obj
    mutable customer: obj
    mutable total: obj
}
let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders: Anon4 list = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 3; total = 80 }]
let result: Anon6 list = [ for o in orders do for c in customers do if (o.customerId) = (c.id) then yield { orderId = o.id; customer = c; total = o.total } ]
printfn "%s" (string "--- Left Join ---")
for entry in result do
printfn "%s" (String.concat " " [string "Order"; string (entry.orderId); string "customer"; string (entry.customer); string "total"; string (entry.total)])
