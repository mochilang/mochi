// Generated 2025-07-21 18:37 +0700

let rec makeAdder n =
    fun x -> (x + n)
let add10 = makeAdder 10
printfn "%s" (string (add10 7))
