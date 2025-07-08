open System

exception Break
exception Continue

let xs = [1; 2; 3]
printfn "%A" (List.contains 2 xs)
printfn "%A" (not (List.contains 5 xs))
