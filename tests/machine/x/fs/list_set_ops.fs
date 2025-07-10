open System

printfn "%s" (String.concat " " (List.map string [1; 2] union [2; 3]))
printfn "%s" (String.concat " " (List.map string [1; 2; 3] except [2]))
printfn "%s" (String.concat " " (List.map string [1; 2; 3] intersect [2; 4]))
printfn "%A" (List.length [1; 2] union [2; 3])
