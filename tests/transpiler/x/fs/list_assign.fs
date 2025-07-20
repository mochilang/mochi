// Generated 2025-07-20 13:26 +0700
open System

let mutable nums = [1; 2]
nums <- List.mapi (fun i x -> (if i = 1 then 3 else x)) nums
printfn "%s" (string (nums.[1]))
