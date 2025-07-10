
type Anon1 = {
    id: int
    name: string
}
type Anon2 = {
    id: int
    customerId: int
}
type Anon3 = {
    name: obj
    count: obj
}
let customers = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }]
let orders = [{ id = 100; customerId = 1 }; { id = 101; customerId = 1 }; { id = 102; customerId = 2 }]
let stats = [ for gKey, gItems in [ for o in orders do 
  for c in customers do if o.customerId = c.id then yield (o, c) ] |> List.groupBy (fun (o, c) -> c.name) do
    let g = {| key = gKey; items = gItems |}
    yield { name = g.key; count = List.length g.items } ]
printfn "%s" "--- Orders per customer ---"
for s in stats do
    printfn "%s" (String.concat " " [string s.name; string "orders:"; string s.count])
