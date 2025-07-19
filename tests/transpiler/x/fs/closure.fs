// Mochi 0.10.31 - generated 2025-07-19 13:08:45 UTC
open System

let rec makeAdder n =
    fun x -> (x + n)
let add10 = makeAdder 10
printfn "%s" (string (add10 7))
