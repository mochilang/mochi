// Generated 2025-07-21 15:37 +0700
open System

let x: int = 8
let msg: string = if x > 10 then "big" else (if x > 5 then "medium" else "small")
printfn "%s" (string msg)
