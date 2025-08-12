// Generated 2025-08-12 13:41 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
open System

let rec intSqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n = 0 then
            __ret <- 0
            raise Return
        let mutable x: int = n
        let mutable y: int = _floordiv (x + 1) 2
        while y < x do
            x <- y
            y <- _floordiv (x + (_floordiv n x)) 2
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and continuousFractionPeriod (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable m: int = 0
        let mutable d: int = 1
        let a0: int = intSqrt (n)
        let mutable a: int = a0
        let mutable period: int = 0
        while a <> (2 * a0) do
            m <- (d * a) - m
            d <- _floordiv (n - (m * m)) d
            a <- _floordiv (a0 + m) d
            period <- period + 1
        __ret <- period
        raise Return
        __ret
    with
        | Return -> __ret
and solution (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable count: int = 0
        for i in 2 .. ((n + 1) - 1) do
            let r: int = intSqrt (i)
            if (r * r) <> i then
                let p: int = continuousFractionPeriod (i)
                if (((p % 2 + 2) % 2)) = 1 then
                    count <- count + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let nStr: string = System.Console.ReadLine()
        let n: int = int (nStr)
        ignore (printfn "%d" (solution (n)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
