// Generated by Mochi compiler v0.10.27 on 2025-07-17T07:48:39Z
open System

let prefix: string = "fore"
let s1: string = "forest"
printfn "%s" (s1.Substring(0, List.length prefix - 0) = prefix)
let s2: string = "desert"
printfn "%s" (s2.Substring(0, List.length prefix - 0) = prefix)
