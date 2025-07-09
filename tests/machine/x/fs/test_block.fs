open System

exception Break
exception Continue

let x = 1 + 2
assert (x = 3)
printfn "%s" "ok"
