open System
open System.Text.Json

exception Break
exception Continue

type Anon1 = {
    a: int
    b: int
}
let m = { a = 1; b = 2 }
printfn "%A" (JsonSerializer.Serialize(m))
