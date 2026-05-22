// Generated 2025-07-21 22:29 +0700

open System

module testpkg
let rec Add a b =
    a + b
let Pi = 3.14
let Answer = 42

printfn "%s" (string (testpkg.Add(2, 3)))
printfn "%s" (string (testpkg.Pi))
printfn "%s" (string (testpkg.Answer))
