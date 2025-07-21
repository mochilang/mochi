// Generated 2025-07-21 12:53 +0700
open System

type Anon1 = {
    name: string
    age: int
    city: string
}
type Anon2 = {
    city: obj
    count: int
    avg_age: float
}
let people: Anon1 list = [Map.ofList [(name, "Alice"); (age, 30); (city, "Paris")]; Map.ofList [(name, "Bob"); (age, 15); (city, "Hanoi")]; Map.ofList [(name, "Charlie"); (age, 65); (city, "Paris")]; Map.ofList [(name, "Diana"); (age, 45); (city, "Hanoi")]; Map.ofList [(name, "Eve"); (age, 70); (city, "Paris")]; Map.ofList [(name, "Frank"); (age, 22); (city, "Hanoi")]]
let stats: Anon2 list = [ for (key, items) in List.groupBy (fun person -> person.city) people do
    let g = {| key = key; items = items |}
    yield Map.ofList [(city, g.key); (count, List.length (g.items)); (avg_age, List.averageBy float [ for p in g.items do yield p.age ])] ]
printfn "%s" (string "--- People grouped by city ---")
for s in stats do
printfn "%s" (String.concat " " [string (s.city); string ": count ="; string (s.count); string ", avg_age ="; string (s.avg_age)])
