open System

exception Break
exception Continue

let m = dict [(1, "a"); (2, "b")]
printfn "%A" (m.[1])
