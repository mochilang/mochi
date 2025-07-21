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
    mutable name: obj
    mutable count: int
}
let customers: Anon2 list = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }]
let orders: Anon4 list = [{ id = 100; customerId = 1 }; { id = 101; customerId = 1 }; { id = 102; customerId = 2 }]
let stats: Anon6 list = [ for (key, items) in List.groupBy (fun { c = c; o = o } -> c.name) [ for c in customers do for o in orders do if (o.customerId) = (c.id) then yield {| c = c; o = o |} ] do
    let g = {| key = key; items = items |}
    yield { name = g.key; count = List.length [ for r in g.items do if r.o then yield r ] } ]
printfn "%s" (string "--- Group Left Join ---")
for s in stats do
printfn "%s" (String.concat " " [string (s.name); string "orders:"; string (s.count)])
