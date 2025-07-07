open System.Text.Json

type Person = { name:string; age:int }

let people = [
    { name = "Alice"; age = 30 }
    { name = "Bob"; age = 25 }
]

for p in people do
    let json = JsonSerializer.Serialize(p)
    printfn "%s" json
