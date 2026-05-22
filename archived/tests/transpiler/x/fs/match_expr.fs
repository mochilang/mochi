// Generated 2025-07-21 18:37 +0700

let x: int = 2
let label: string = match x with
| 1 -> "one"
| 2 -> "two"
| 3 -> "three"
| _ -> "unknown"
printfn "%s" (string label)
