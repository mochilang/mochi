open System

printfn "%s" (String.concat " " (List.map string [1; 2; 3].[1]))
printfn "%s" (String.concat " " (List.map string [1; 2; 3].[0]))
printfn "%A" ("hello".[1])
