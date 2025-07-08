open System

exception Break
exception Continue

let square = fun x -> x * x
printfn "%A" (square 6)
