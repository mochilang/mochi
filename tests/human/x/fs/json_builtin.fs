open System.Text.Json

let m = Map.ofList [ "a", 1; "b", 2 ]
let json = JsonSerializer.Serialize(m)
printfn "%s" json
