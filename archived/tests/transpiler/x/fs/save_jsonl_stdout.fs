// Generated 2025-07-22 05:29 +0700

type Anon1 = {
    name: string
    age: int
}
type Anon2 = {
    name: string
    age: int
}
open System

open System.Text.Json

let people: Anon2 list = [{ name = "Alice"; age = 30 }; { name = "Bob"; age = 25 }]
(List.iter (fun row -> printfn "%s" (JsonSerializer.Serialize(row))) people)
