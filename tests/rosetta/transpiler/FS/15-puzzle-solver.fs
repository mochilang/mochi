// Generated 2025-07-24 13:03 +0700

open System

module testpkg
    let rec Add a b =
        let mutable __ret = ()
        let mutable a = a
        let mutable b = b
        try
            __ret <- a + b
            raise Return
            __ret
        with
            | Return -> __ret
    let Pi = 3.14
    let Answer = 42

printfn "%s" (string (testpkg.FifteenPuzzleExample()))
