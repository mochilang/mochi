open System

exception Break
exception Continue

let mutable x = 1
x <- 2
printfn "%A" (x)
