// Generated 2025-08-01 15:22 +0700

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
let rec floyd (n: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    try
        printfn "%s" (("Floyd " + (string n)) + ":")
        let lowerLeftCorner: int = ((n * (n - 1)) / 2) + 1
        let mutable lastInColumn: int = lowerLeftCorner
        let mutable lastInRow: int = 1
        let mutable i: int = 1
        let mutable row: int = 1
        let mutable line: string = ""
        while row <= n do
            let w: int = String.length (string lastInColumn)
            if i < lastInRow then
                line <- (line + (unbox<string> (pad (string i) w))) + " "
                lastInColumn <- lastInColumn + 1
            else
                line <- line + (unbox<string> (pad (string i) w))
                printfn "%s" line
                line <- ""
                row <- row + 1
                lastInRow <- lastInRow + row
                lastInColumn <- lowerLeftCorner
            i <- i + 1
        __ret
    with
        | Return -> __ret
and pad (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable t: string = s
        while (String.length t) < w do
            t <- " " + t
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
floyd 5
floyd 14
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
