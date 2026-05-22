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
    mutable orderCustomerId: obj
    mutable pairedCustomerName: obj
    mutable orderTotal: obj
}
type Anon6 = {
    mutable orderId: obj
    mutable orderCustomerId: obj
    mutable pairedCustomerName: obj
    mutable orderTotal: obj
}
let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }]
let orders: Anon4 list = [{ id = 100; customerId = 1; total = 250 }; { id = 101; customerId = 2; total = 125 }; { id = 102; customerId = 1; total = 300 }]
let result: Anon6 list = [ for o in orders do for c in customers do yield { orderId = o.id; orderCustomerId = o.customerId; pairedCustomerName = c.name; orderTotal = o.total } ]
printfn "%s" (string "--- Cross Join: All order-customer pairs ---")
for entry in result do
printfn "%s" (String.concat " " [string "Order"; string (entry.orderId); string "(customerId:"; string (entry.orderCustomerId); string ", total: $"; string (entry.orderTotal); string ") paired with"; string (entry.pairedCustomerName)])
