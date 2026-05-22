// Generated 2025-07-22 06:32 +0700

type Person = {
    name: string
    age: int
    email: string
}
type Anon1 = {
    name: obj
    email: obj
}
open System

open System.IO

open System.Text.Json

let people = (File.ReadLines("../interpreter/valid/people.jsonl") |> Seq.map (fun line -> JsonSerializer.Deserialize<Person>(line)) |> Seq.toList)
let adults: Anon1 list = [ for p in people do if (p.age) >= 18 then yield Map.ofList [(name, p.name); (email, p.email)] ]
for a in adults do
printfn "%s" (String.concat " " [string (a.name); string (a.email)])
