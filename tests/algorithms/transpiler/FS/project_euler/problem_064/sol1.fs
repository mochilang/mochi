// Generated 2025-08-23 01:21 +0700

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
let _readLine () =
    match System.Console.ReadLine() with
    | null -> ""
    | s -> s
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
open System

let rec intSqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n = 0 then
            __ret <- 0
            raise Return
        let mutable x: int = n
        let mutable y: int = _floordiv (int (x + 1)) (int 2)
        while y < x do
            x <- y
            y <- _floordiv (int (x + (_floordiv (int n) (int x)))) (int 2)
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
        while (int64 a) <> ((int64 2) * (int64 a0)) do
            m <- int (((int64 d) * (int64 a)) - (int64 m))
            d <- int (_floordiv64 (int64 ((int64 n) - ((int64 m) * (int64 m)))) (int64 (int64 d)))
            a <- _floordiv (int (a0 + m)) (int d)
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
            if ((int64 r) * (int64 r)) <> (int64 i) then
                let p: int = continuousFractionPeriod (i)
                if (((p % 2 + 2) % 2)) = 1 then
                    count <- count + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let nStr: string = _readLine()
        let n: int = int (nStr)
        ignore (printfn "%d" (solution (n)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
