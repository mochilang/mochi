open System

let makeAdder n = fun x -> x + n
let add10 = makeAdder 10
printfn "%d" (add10 7)
