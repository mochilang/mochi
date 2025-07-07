open System

type Person = { name: string; age: int }

let people = [
    { name = "Alice"; age = 30 }
    { name = "Bob"; age = 15 }
    { name = "Charlie"; age = 65 }
    { name = "Diana"; age = 45 }
]

let adults =
    [ for p in people do
        if p.age >= 18 then
            yield {| name = p.name; age = p.age; is_senior = p.age >= 60 |} ]

printfn "--- Adults ---"
for person in adults do
    let suffix = if person.is_senior then " (senior)" else ""
    printfn "%s is %d%s" person.name person.age suffix

