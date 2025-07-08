open System

exception Break
exception Continue

let x = 12
let msg = (if x > 10 then "yes" else "no")
printfn "%A" (msg)
