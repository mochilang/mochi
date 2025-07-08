open System

exception Break
exception Continue

let add (a) (b) =
    a + b
printfn "%A" (add 2 3)
