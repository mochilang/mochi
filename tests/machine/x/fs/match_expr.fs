open System

let x: int = 2
let label: obj = (match x with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | _ -> "unknown")
printfn "%A" (label)
