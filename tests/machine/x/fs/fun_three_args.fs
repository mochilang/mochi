open System

exception Break
exception Continue

let sum3 (a) (b) (c) =
    a + b + c
printfn "%A" (sum3 1 2 3)
