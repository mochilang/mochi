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
}
type Anon4 = {
    mutable id: int
    mutable customerId: int
}
type Anon5 = {
    mutable orderId: int
    mutable sku: string
}
type Anon6 = {
    mutable orderId: int
    mutable sku: string
}
type Anon7 = {
    mutable name: obj
    mutable sku: obj
}
type Anon8 = {
    mutable name: obj
    mutable sku: obj
}
let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders: Anon4 list = [{ id = 100; customerId = 1 }; { id = 101; customerId = 2 }]
let items: Anon6 list = [{ orderId = 100; sku = "a" }; { orderId = 101; sku = "b" }]
let result: Anon8 list = [ for o in orders do for c in customers do if (o.customerId) = (c.id) then for i in items do if (o.id) = (i.orderId) then yield { name = c.name; sku = i.sku } ]
printfn "%s" (string "--- Multi Join ---")
for r in result do
printfn "%s" (String.concat " " [string (r.name); string "bought item"; string (r.sku)])
