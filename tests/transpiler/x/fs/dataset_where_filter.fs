// Generated 2025-07-21 15:07 +0700
open System

type Anon1 = {
    mutable name: string
    mutable age: int
}
type Anon2 = {
    mutable name: string
    mutable age: int
}
type Anon3 = {
    mutable name: obj
    mutable age: obj
    mutable is_senior: bool
}
type Anon4 = {
    mutable name: obj
    mutable age: obj
    mutable is_senior: bool
}
let people: Anon2 list = [{ name = "Alice"; age = 30 }; { name = "Bob"; age = 15 }; { name = "Charlie"; age = 65 }; { name = "Diana"; age = 45 }]
let adults: Anon4 list = [ for person in people do if (person.age) >= 18 then yield { name = person.name; age = person.age; is_senior = (person.age) >= 60 } ]
printfn "%s" (string "--- Adults ---")
for person in adults do
printfn "%s" (String.concat " " [string (person.name); string "is"; string (person.age); string (if person.is_senior then " (senior)" else "")])
