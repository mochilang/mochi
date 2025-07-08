open System

exception Break
exception Continue

let x = 8
let msg = (if x > 10 then "big" else (if x > 5 then "medium" else "small"))
printfn "%A" (msg)
