open System

let avg xs = List.sum xs / List.length xs
printfn "%d" (avg [1; 2; 3])
