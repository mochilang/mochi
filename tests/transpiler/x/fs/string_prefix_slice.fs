// Generated 2025-07-20 10:18 +0700
open System

let prefix: string = "fore"
let s1: string = "forest"
printfn "%b" ((s1.Substring(0, (String.length prefix) - 0)) = prefix)
let s2: string = "desert"
printfn "%b" ((s2.Substring(0, (String.length prefix) - 0)) = prefix)
