open System

let x: int = 8
let msg: obj = (if x > 10 then "big" else (if x > 5 then "medium" else "small"))
printfn "%A" (msg)
