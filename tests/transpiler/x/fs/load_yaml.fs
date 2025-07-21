// Generated 2025-07-22 05:29 +0700

type Person = {
    name: string
    age: int
    email: string
}
type Anon1 = {
    name: obj
    email: obj
}
type Anon2 = {
    name: obj
    email: obj
}
open System

open System.IO

open YamlDotNet.Serialization

let people = (let deserializer = DeserializerBuilder().Build()
    let yamlText = File.ReadAllText("../interpreter/valid/people.yaml")
    deserializer.Deserialize<Person list>(yamlText))
let adults: Anon2 list = [ for p in people do if (p.age) >= 18 then yield { name = p.name; email = p.email } ]
for a in adults do
printfn "%s" (String.concat " " [string (a.name); string (a.email)])
