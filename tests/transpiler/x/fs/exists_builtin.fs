// Generated 2025-07-20 22:05 +0700
open System

let data = [1; 2]
let flag: bool = not (List.isEmpty [ for x in data do if x = 1 then yield x ])
printfn "%b" flag
