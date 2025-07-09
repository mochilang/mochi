open System

exception Break
exception Continue

let mutable x: int = 0
printfn "%A" (x)
