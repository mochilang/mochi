// Generated 2025-07-21 18:37 +0700

let x: int = 2
let label: string = match x with
| 1 -> "one"
| 2 -> "two"
| 3 -> "three"
| _ -> "unknown"
printfn "%s" (string label)
let day: string = "sun"
let mood: string = match day with
| "mon" -> "tired"
| "fri" -> "excited"
| "sun" -> "relaxed"
| _ -> "normal"
printfn "%s" (string mood)
let ok: bool = true
let status: string = match ok with
| true -> "confirmed"
| false -> "denied"
printfn "%s" (string status)
let rec classify n =
    match n with
| 0 -> "zero"
| 1 -> "one"
| _ -> "many"
printfn "%s" (string (classify 0))
printfn "%s" (string (classify 5))
