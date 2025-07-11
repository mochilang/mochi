open System
open System.Text.Json

type Anon1 = {
    a: int
    b: int
}
let m: Anon1 = { a = 1; b = 2 }
printfn "%A" (JsonSerializer.Serialize(m))
