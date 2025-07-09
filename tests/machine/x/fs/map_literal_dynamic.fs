open System

exception Break
exception Continue

let mutable x = 3
let mutable y = 4
let mutable m = dict [("a", x); ("b", y)]
printfn "%s" (String.concat " " [string m.["a"]; string m.["b"]])
