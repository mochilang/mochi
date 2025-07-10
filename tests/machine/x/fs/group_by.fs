
type Anon1 = {
    name: string
    age: int
    city: string
}
type Anon2 = {
    city: obj
    count: obj
    avg_age: obj
}
let people = [{ name = "Alice"; age = 30; city = "Paris" }; { name = "Bob"; age = 15; city = "Hanoi" }; { name = "Charlie"; age = 65; city = "Paris" }; { name = "Diana"; age = 45; city = "Hanoi" }; { name = "Eve"; age = 70; city = "Paris" }; { name = "Frank"; age = 22; city = "Hanoi" }]
let stats = [ for gKey, gItems in [ for person in people do yield person ] |> List.groupBy (fun person -> person.city) do
    let g = {| key = gKey; items = gItems |}
    yield { city = g.key; count = List.length g.items; avg_age = (List.sum [ for p in g do yield p.age ] / List.length [ for p in g do yield p.age ]) } ]
printfn "%s" "--- People grouped by city ---"
for s in stats do
    printfn "%s" (String.concat " " [string s.city; string ": count ="; string s.count; string ", avg_age ="; string s.avg_age])
