open System

let x: int = 12
let msg: string = (if x > 10 then "yes" else "no")
printfn "%s" msg
