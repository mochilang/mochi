open System

exception Break
exception Continue

let s: string = "catch"
printfn "%s" List.contains "cat" s
printfn "%s" List.contains "dog" s
