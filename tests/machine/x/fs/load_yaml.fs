open System
open System.IO
open YamlDotNet.Serialization

type Anon1 = {
    name: obj
    email: obj
}
type Person = {
    mutable name: string
    mutable age: int
    mutable email: string
}
let people = (let deserializer = DeserializerBuilder().Build()
    let yamlText = File.ReadAllText("../interpreter/valid/people.yaml")
    deserializer.Deserialize<Person list>(yamlText))
let adults = [ for p in people do if p.age >= 18 then yield { name = p.name; email = p.email } ]
for a in adults do
    printfn "%s" (String.concat " " [string a.name; string a.email])
