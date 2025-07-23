// Generated 2025-07-23 02:25 +0000

exception Break
exception Continue

let mutable _nowSeed = 0
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525 + 1013904223) % 2147483647
        _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
open System

let rec shuffle xs =
    let mutable arr = xs
    let mutable i: int = 99
    while i > 0 do
        let j = (_now()) % (i + 1)
        let tmp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- tmp
        i <- i - 1
    arr
let rec doTrials (trials: int) (np: int) (strategy: string) =
    let mutable pardoned: int = 0
    let mutable t: int = 0
    try
        while t < trials do
            let mutable drawers = [||]
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
                                d <- d + 1                        with
                        | Break -> ()
                    else
                        let mutable opened = [||]
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
                                d <- d + 1                        with
                        | Break -> ()
                    if not found then
                        success <- false
                        raise Break
                    p <- p + 1            with
            | Break -> ()
            if success then
                pardoned <- pardoned + 1
            t <- t + 1    with
    | Break -> ()
    let rf = ((float pardoned) / (float trials)) * 100.0
    printfn "%s" (((((("  strategy = " + strategy) + "  pardoned = ") + (string pardoned)) + " relative frequency = ") + (string rf)) + "%")
let rec main () =
    let trials: int = 1000
    for np in [|10; 100|] do
        printfn "%s" (((("Results from " + (string trials)) + " trials with ") + (string np)) + " prisoners:\n")
        for strat in [|"random"; "optimal"|] do
            doTrials trials np strat
main()
