// Generated 2025-07-21 18:37 +0700

printfn "%s" (string ([1; 2] union [2; 3]))
printfn "%s" (string ([1; 2; 3] except [2]))
printfn "%s" (string ([1; 2; 3] intersect [2; 4]))
printfn "%d" (Seq.length ([1; 2] union [2; 3]))
