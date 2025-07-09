open System

exception Break
exception Continue

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
let customers = [{ id = 1; name = "Alice" }; { id = 2; name = "Bob" }; { id = 3; name = "Charlie" }]
let orders = [{ id = 100; customerId = 1 }; { id = 101; customerId = 1 }; { id = 102; customerId = 2 }]
let stats = [ for gKey, gItems in [ for c in customers do 
  for o in orders do if o.customerId = c.id then yield (c, o) ] |> List.groupBy (fun (c, o) -> c.name) do
    let g = {| key = gKey; items = gItems |}
    yield { name = g.key; count = List.length [ for r in g do if r.o then yield r ] } ]
printfn "%s" "--- Group Left Join ---"
try
    for s in stats do
        try
            printfn "%s" (String.concat " " [string s.name; string "orders:"; string s.count])
        with Continue -> ()
with Break -> ()
