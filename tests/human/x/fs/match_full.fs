let x = 2

let label =
    match x with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | _ -> "unknown"

printfn "%s" label

let day = "sun"

let mood =
    match day with
    | "mon" -> "tired"
    | "fri" -> "excited"
    | "sun" -> "relaxed"
    | _ -> "normal"

printfn "%s" mood

let ok = true

let status = if ok then "confirmed" else "denied"
printfn "%s" status

let classify n =
    match n with
    | 0 -> "zero"
    | 1 -> "one"
    | _ -> "many"

printfn "%s" (classify 0)
printfn "%s" (classify 5)
