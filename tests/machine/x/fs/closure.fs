open System

exception Break
exception Continue

let makeAdder (n) =
    fun x -> x + n
let add10 = makeAdder 10
printfn "%A" (add10 7)
