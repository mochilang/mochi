open System

let makeAdder (n) =
    fun x -> x + n
let add10: obj = makeAdder 10
printfn "%A" (add10 7)
