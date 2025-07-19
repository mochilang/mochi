// Mochi 0.10.31 - generated 2025-07-19 13:09:09 UTC
open System

printfn "%s" (string ([1; 2] union [2; 3]))
printfn "%s" (string ([1; 2; 3] except [2]))
printfn "%s" (string ([1; 2; 3] intersect [2; 4]))
printfn "%s" (string (Seq.length ([1; 2] union [2; 3])))
