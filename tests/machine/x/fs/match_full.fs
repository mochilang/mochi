open System

let x: int = 2
let label: obj = (match x with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | _ -> "unknown")
printfn "%A" (label)
let day: string = "sun"
let mood: obj = (match day with
    | "mon" -> "tired"
    | "fri" -> "excited"
    | "sun" -> "relaxed"
    | _ -> "normal")
printfn "%A" (mood)
let ok: bool = true
let status: obj = (match ok with
    | true -> "confirmed"
    | false -> "denied")
printfn "%A" (status)
let classify (n) =
    (match n with
    | 0 -> "zero"
    | 1 -> "one"
    | _ -> "many")
printfn "%A" (classify 0)
printfn "%A" (classify 5)
