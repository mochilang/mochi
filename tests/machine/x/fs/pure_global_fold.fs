open System

exception Break
exception Continue

let k = 2
let inc (x) =
    x + k
printfn "%A" (inc 3)
