// Generated 2025-07-21 15:37 +0700
open System

type Anon1 = {
    mutable name: string
    mutable age: int
    mutable city: string
}
type Anon2 = {
    mutable name: string
    mutable age: int
    mutable city: string
}
type Anon3 = {
    mutable city: obj
    mutable count: int
    mutable avg_age: float
}
type Anon4 = {
    mutable city: obj
    mutable count: int
    mutable avg_age: float
}
let people: Anon2 list = [{ name = "Alice"; age = 30; city = "Paris" }; { name = "Bob"; age = 15; city = "Hanoi" }; { name = "Charlie"; age = 65; city = "Paris" }; { name = "Diana"; age = 45; city = "Hanoi" }; { name = "Eve"; age = 70; city = "Paris" }; { name = "Frank"; age = 22; city = "Hanoi" }]
let stats: Anon4 list = [ for (key, items) in List.groupBy (fun person -> person.city) people do
    let g = {| key = key; items = items |}
    yield { city = g.key; count = List.length (g.items); avg_age = List.averageBy float [ for p in g.items do yield p.age ] } ]
printfn "%s" (string "--- People grouped by city ---")
for s in stats do
printfn "%s" (String.concat " " [string (s.city); string ": count ="; string (s.count); string ", avg_age ="; string (s.avg_age)])
