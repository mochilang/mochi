open System
open System.IO
open YamlDotNet.Serialization

[<CLIMutable>]
type Person = { name: string; age: int; email: string }

let deserializer = DeserializerBuilder().Build()
let yamlText = File.ReadAllText("../../interpreter/valid/people.yaml")
let people = deserializer.Deserialize<Person list>(yamlText)

let adults =
    people
    |> List.filter (fun p -> p.age >= 18)
    |> List.map (fun p -> {| name = p.name; email = p.email |})

for a in adults do
    printfn "%s %s" a.name a.email
