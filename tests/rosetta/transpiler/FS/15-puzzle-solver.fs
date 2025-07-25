// Generated 2025-07-25 08:23 +0700

exception Return

module testpkg =
    open System
    let rec Add a b =
        let mutable __ret : int = Unchecked.defaultof<int>
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
    let rec FifteenPuzzleExample () =
        let mutable __ret : string = Unchecked.defaultof<string>
        try
            __ret <- "Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd"
            raise Return
            __ret
        with
            | Return -> __ret

printfn "%A" (testpkg.FifteenPuzzleExample())
