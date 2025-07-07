let x = 2

let label =
    match x with
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | _ -> "unknown"

printfn "%s" label
