open System

let mutable x: int = 3
let mutable y: int = 4
let mutable m: System.Collections.Generic.IDictionary<string, int> = dict [("a", x); ("b", y)]
printfn "%s" (String.concat " " [string m.["a"]; string m.["b"]])
