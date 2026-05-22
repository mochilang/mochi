// Generated 2025-07-21 18:37 +0700

let rec boom a b =
    printfn "%s" (string "boom")
    true
printfn "%b" (false && (boom 1 2))
printfn "%b" (true || (boom 1 2))
