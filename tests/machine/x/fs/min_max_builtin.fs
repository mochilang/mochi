open System

exception Break
exception Continue

let nums = [3; 1; 4]
printfn "%A" (List.min nums)
printfn "%A" (List.max nums)
