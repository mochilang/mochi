open System

exception Break
exception Continue

let s: string = "catch"
printfn "%s" s.contains("cat")
printfn "%s" s.contains("dog")
