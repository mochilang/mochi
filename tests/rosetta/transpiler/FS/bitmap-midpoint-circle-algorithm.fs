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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec initGrid (size: int) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable size = size
    try
        let mutable g: string array array = [||]
        let mutable y: int = 0
        while y < size do
            let mutable row: string array = [||]
            let mutable x: int = 0
            while x < size do
                row <- unbox<string array> (Array.append row [|" "|])
                x <- x + 1
            g <- unbox<string array array> (Array.append g [|row|])
            y <- y + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
let rec set (g: string array array) (x: int) (y: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable x = x
    let mutable y = y
    try
        if (((x >= 0) && (x < (Seq.length (g.[0])))) && (y >= 0)) && (y < (unbox<int> (Array.length g))) then
            (g.[y]).[x] <- "#"
        __ret
    with
        | Return -> __ret
let rec circle (r: int) =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    let mutable r = r
    try
        let size: int = (r * 2) + 1
        let mutable g: string array array = initGrid size
        let mutable x: int = r
        let mutable y: int = 0
        let mutable err: int = 1 - r
        while y <= x do
            set g (r + x) (r + y)
            set g (r + y) (r + x)
            set g (r - x) (r + y)
            set g (r - y) (r + x)
            set g (r - x) (r - y)
            set g (r - y) (r - x)
            set g (r + x) (r - y)
            set g (r + y) (r - x)
            y <- y + 1
            if err < 0 then
                err <- (err + (2 * y)) + 1
            else
                x <- x - 1
                err <- (err + (2 * (y - x))) + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
let rec trimRight (row: string array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    try
        let mutable ``end``: int = Array.length row
        while (``end`` > 0) && ((unbox<string> (row.[``end`` - 1])) = " ") do
            ``end`` <- ``end`` - 1
        let mutable s: string = ""
        let mutable i: int = 0
        while i < ``end`` do
            s <- s + (unbox<string> (row.[i]))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let mutable g: string array array = circle 10
for row in g do
    printfn "%A" (trimRight (unbox<string array> row))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
