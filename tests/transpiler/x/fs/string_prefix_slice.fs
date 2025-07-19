// Mochi 0.10.31 - generated 2025-07-19 13:09:32 UTC
open System

let prefix = "fore"
let s1 = "forest"
printfn "%s" (string ((s1.Substring(0, (Seq.length prefix) - 0)) = prefix))
let s2 = "desert"
printfn "%s" (string ((s2.Substring(0, (Seq.length prefix) - 0)) = prefix))
