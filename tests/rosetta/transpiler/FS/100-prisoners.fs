// Generated 2025-07-25 12:29 +0700

exception Break
exception Continue

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
open System

let rec shuffle (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr = xs
        let mutable i: int = 99
        while i > 0 do
            let j = (_now()) % (i + 1)
            let tmp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- tmp
            i <- i - 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and doTrials (trials: int) (np: int) (strategy: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable trials = trials
    let mutable np = np
    let mutable strategy = strategy
    try
        let mutable pardoned: int = 0
        let mutable t: int = 0
        try
            while t < trials do
                let mutable drawers: int array = [||]
                let mutable i: int = 0
                while i < 100 do
                    drawers <- Array.append drawers [|i|]
                    i <- i + 1
                drawers <- shuffle drawers
                let mutable p: int = 0
                let mutable success: bool = true
                try
                    while p < np do
                        let mutable found: bool = false
                        if strategy = "optimal" then
                            let mutable prev: int = p
                            let mutable d: int = 0
                            try
                                while d < 50 do
                                    let this = drawers.[prev]
                                    if this = p then
                                        found <- true
                                        raise Break
                                    prev <- this
                                    d <- d + 1
                            with
                            | Break -> ()
                            | Continue -> ()
                        else
                            let mutable opened: bool array = [||]
                            let mutable k: int = 0
                            while k < 100 do
                                opened <- Array.append opened [|false|]
                                k <- k + 1
                            let mutable d: int = 0
                            try
                                while d < 50 do
                                    let mutable n = (_now()) % 100
                                    while opened.[n] do
                                        n <- (_now()) % 100
                                    opened.[n] <- true
                                    if (drawers.[n]) = p then
                                        found <- true
                                        raise Break
                                    d <- d + 1
                            with
                            | Break -> ()
                            | Continue -> ()
                        if not found then
                            success <- false
                            raise Break
                        p <- p + 1
                with
                | Break -> ()
                | Continue -> ()
                if success then
                    pardoned <- pardoned + 1
                t <- t + 1
        with
        | Break -> ()
        | Continue -> ()
        let rf = ((float pardoned) / (float trials)) * 100.0
        printfn "%s" (((((("  strategy = " + strategy) + "  pardoned = ") + (string pardoned)) + " relative frequency = ") + (string rf)) + "%")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let trials: int = 1000
        for np in [|10; 100|] do
            printfn "%s" (((("Results from " + (string trials)) + " trials with ") + (string np)) + " prisoners:\n")
            for strat in [|"random"; "optimal"|] do
                doTrials trials np strat
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
