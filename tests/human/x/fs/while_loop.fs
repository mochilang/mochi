open System

let mutable i = 0
while i < 3 do
    printfn "%d" i
    i <- i + 1
