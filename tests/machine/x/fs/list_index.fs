open System

exception Break
exception Continue

let xs = [10; 20; 30]
printfn "%A" (xs.[1])
