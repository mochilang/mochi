open System

exception Break
exception Continue

type Person = {
    mutable name: string
    mutable age: int
    mutable status: string
}
let people = [{ name = "Alice"; age = 17; status = "minor" }; { name = "Bob"; age = 25; status = "unknown" }; { name = "Charlie"; age = 18; status = "unknown" }; { name = "Diana"; age = 16; status = "minor" }]
for item in people do
    if item.age >= 18 then
        item.status <- "adult"
        item.age <- item.age + 1
assert (people = [{ name = "Alice"; age = 17; status = "minor" }; { name = "Bob"; age = 26; status = "adult" }; { name = "Charlie"; age = 19; status = "adult" }; { name = "Diana"; age = 16; status = "minor" }])
printfn "%s" "ok"
