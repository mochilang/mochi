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
    mutable name: obj
    mutable count: int
}
type Anon6 = {
    mutable o: obj
    mutable c: obj
}
type Anon7 = {
    mutable key: obj
    mutable items: Anon6 list
}
type Anon8 = {
    mutable name: obj
    mutable count: int
}
let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders: Anon4 list = [{ id = 100; customerId = 1 }; { id = 101; customerId = 1 }; { id = 102; customerId = 2 }]
let stats: Anon8 list = [ for (key, items) in List.groupBy (fun { o = o; c = c } -> c.name) [ for o in orders do for c in customers do if (o.customerId) = (c.id) then yield { o = o; c = c } : Anon6 ] do
    let g : Anon7 = { key = key; items = items }
    yield { name = g.key; count = List.length (g.items) } ]
printfn "%s" (string "--- Orders per customer ---")
for s in stats do
printfn "%s" (String.concat " " [string (s.name); string "orders:"; string (s.count)])
