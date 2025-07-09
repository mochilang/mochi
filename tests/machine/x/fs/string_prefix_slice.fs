open System

exception Break
exception Continue

let prefix: string = "fore"
let s1: string = "forest"
printfn "%s" s1.Substring(0, List.length prefix - 0) = prefix
let s2: string = "desert"
printfn "%s" s2.Substring(0, List.length prefix - 0) = prefix
