open System

exception Break
exception Continue

let x = 5
if x > 3 then
    printfn "%s" "big"
else
    printfn "%s" "small"
