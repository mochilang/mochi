open System

type Anon1 = {
    name: string
    age: int
}
type Anon2 = {
    name: obj
    age: obj
    is_senior: bool
}
let people = [{ name = "Alice"; age = 30 }; { name = "Bob"; age = 15 }; { name = "Charlie"; age = 65 }; { name = "Diana"; age = 45 }]
let adults = [ for person in people do if person.age >= 18 then yield { name = person.name; age = person.age; is_senior = person.age >= 60 } ]
printfn "%s" "--- Adults ---"
for person in adults do
    printfn "%s" (String.concat " " [string person.name; string "is"; string person.age; string (if person.is_senior then " (senior)" else "")])
