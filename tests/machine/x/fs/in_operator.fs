open System

let xs = [1; 2; 3]
printfn "%b" (List.contains 2 xs)
printfn "%b" (not (List.contains 5 xs))
