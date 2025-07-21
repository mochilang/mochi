// Generated 2025-07-21 15:37 +0700
open System

let data: int list = [1; 2]
let flag: bool = not (List.isEmpty [ for x in data do if x = 1 then yield x ])
printfn "%b" flag
