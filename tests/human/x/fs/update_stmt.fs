open System

type Person = { name:string; mutable age:int; mutable status:string }

let people = [
    { name = "Alice"; age = 17; status = "minor" }
    { name = "Bob"; age = 25; status = "unknown" }
    { name = "Charlie"; age = 18; status = "unknown" }
    { name = "Diana"; age = 16; status = "minor" }
]

for p in people do
    if p.age >= 18 then
        p.status <- "adult"
        p.age <- p.age + 1

let expected = [
    { name = "Alice"; age = 17; status = "minor" }
    { name = "Bob"; age = 26; status = "adult" }
    { name = "Charlie"; age = 19; status = "adult" }
    { name = "Diana"; age = 16; status = "minor" }
]

if people = expected then
    printfn "ok"
else
    printfn "mismatch"
