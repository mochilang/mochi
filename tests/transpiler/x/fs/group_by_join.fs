// Generated 2025-07-22 04:52 +0700

type Anon1 = {
    id: int
    name: string
}
type Anon2 = {
    id: int
    name: string
}
type Anon3 = {
    id: int
    customerId: int
}
type Anon4 = {
    id: int
    customerId: int
}
type Anon5 = {
    name: obj
    count: int
}
type Anon6 = {
    o: Anon4
    c: Anon2
}
type Anon7 = {
    key: string
    items: Anon6 list
}
type Anon8 = {
    name: obj
    count: int
}
let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders: Anon4 list = [{ id = 100; customerId = 1 }; { id = 101; customerId = 1 }; { id = 102; customerId = 2 }]
let stats: Anon8 list = [ for (key, items) in List.groupBy (fun { o = o; c = c } -> c.name) [ for o in orders do for c in customers do if (o.customerId) = (c.id) then yield { o = o; c = c } : Anon6 ] do
    let g : Anon7 = { key = key; items = items }
    yield { name = g.key; count = List.length (g.items) } ]
printfn "%s" "--- Orders per customer ---"
for s in stats do
printfn "%s" (String.concat " " [string (s.name); string "orders:"; string (s.count)])
