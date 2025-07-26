// Generated 2025-07-26 03:08 +0000

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
let rec image () =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    try
        __ret <- [|[|0; 0; 10000|]; [|65535; 65535; 65535|]; [|65535; 65535; 65535|]|]
        raise Return
        __ret
    with
        | Return -> __ret
and histogram (g: int array array) (bins: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable g = g
    let mutable bins = bins
    try
        if bins <= 0 then
            bins <- Seq.length (g.[0])
        let mutable h: int array = [||]
        let mutable i: int = 0
        while i < bins do
            h <- unbox<int array> (Array.append h [|0|])
            i <- i + 1
        let mutable y: int = 0
        while y < (unbox<int> (Array.length g)) do
            let mutable row: int array = g.[y]
            let mutable x: int = 0
            while x < (unbox<int> (Array.length row)) do
                let mutable p: int = row.[x]
                let mutable idx: int = int ((p * (bins - 1)) / 65535)
                h.[idx] <- (unbox<int> (h.[idx])) + 1
                x <- x + 1
            y <- y + 1
        __ret <- h
        raise Return
        __ret
    with
        | Return -> __ret
and medianThreshold (h: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    try
        let mutable lb: int = 0
        let mutable ub: int = (unbox<int> (Array.length h)) - 1
        let mutable lSum: int = 0
        let mutable uSum: int = 0
        while lb <= ub do
            if (lSum + (unbox<int> (h.[lb]))) < (uSum + (unbox<int> (h.[ub]))) then
                lSum <- unbox<int> (lSum + (unbox<int> (h.[lb])))
                lb <- lb + 1
            else
                uSum <- unbox<int> (uSum + (unbox<int> (h.[ub])))
                ub <- ub - 1
        __ret <- unbox<int> ((ub * 65535) / (unbox<int> (Array.length h)))
        raise Return
        __ret
    with
        | Return -> __ret
and threshold (g: int array array) (t: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable g = g
    let mutable t = t
    try
        let mutable out: int array array = [||]
        let mutable y: int = 0
        while y < (unbox<int> (Array.length g)) do
            let mutable row: int array = g.[y]
            let mutable newRow: int array = [||]
            let mutable x: int = 0
            while x < (unbox<int> (Array.length row)) do
                if (unbox<int> (row.[x])) < t then
                    newRow <- unbox<int array> (Array.append newRow [|0|])
                else
                    newRow <- unbox<int array> (Array.append newRow [|65535|])
                x <- x + 1
            out <- unbox<int array array> (Array.append out [|newRow|])
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and printImage (g: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    try
        let mutable y: int = 0
        while y < (unbox<int> (Array.length g)) do
            let mutable row: int array = g.[y]
            let mutable line: string = ""
            let mutable x: int = 0
            while x < (unbox<int> (Array.length row)) do
                if (unbox<int> (row.[x])) = 0 then
                    line <- line + "0"
                else
                    line <- line + "1"
                x <- x + 1
            printfn "%s" line
            y <- y + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let img: int array array = image()
        let h: int array = histogram img 0
        printfn "%s" ("Histogram: " + (string h))
        let t: int = medianThreshold h
        printfn "%s" ("Threshold: " + (string t))
        let bw: int array array = threshold img t
        printImage bw
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
