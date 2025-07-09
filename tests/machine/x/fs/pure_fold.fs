open System

exception Break
exception Continue

let triple (x) =
    x * 3
printfn "%A" (triple 1 + 2)
