// Generated 2025-07-20 13:26 +0700
open System

let mutable matrix = [[1; 2]; [3; 4]]
matrix <- List.mapi (fun i x -> (if i = 1 then (List.mapi (fun i x -> (if i = 0 then 5 else x)) (matrix.[1])) else x)) matrix
printfn "%s" (string (matrix.[1].[0]))
