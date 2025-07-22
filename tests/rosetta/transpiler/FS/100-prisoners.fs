// Generated 2025-07-22 22:23 +0700

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
        let tmp = List.item i arr
        arr <- List.mapi (fun k x -> if k = i then List.item j arr else x) arr
        arr <- List.mapi (fun k x -> if k = j then tmp else x) arr
        i <- i - 1
    arr
let rec doTrials trials np strategy =
    let mutable pardoned: int = 0
    let mutable t: int = 0
    try
        while t < trials do
            let mutable drawers = []
            let mutable i: int = 0
            while i < 100 do
                drawers <- drawers @ [i]
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
                                let this = List.item prev drawers
                                if this = p then
                                    found <- true
                                    raise Break
                                prev <- this
                                d <- d + 1                        with
                        | Break -> ()
                    else
                        let mutable opened = []
                        let mutable k: int = 0
                        while k < 100 do
                            opened <- opened @ [false]
                            k <- k + 1
                        let mutable d: int = 0
                        try
                            while d < 50 do
                                let mutable n = (_now()) % 100
                                while List.item n opened do
                                    n <- (_now()) % 100
                                opened <- List.mapi (                                fun i x -> (if i = n then true else x)) opened
                                if (List.item n drawers) = p then
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
    for np in [10; 100] do
        printfn "%s" (((("Results from " + (string trials)) + " trials with ") + (string np)) + " prisoners:\n")
        for strat in ["random"; "optimal"] do
            doTrials trials np strat
main()
