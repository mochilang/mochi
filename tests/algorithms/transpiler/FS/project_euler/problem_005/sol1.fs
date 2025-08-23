// Generated 2025-08-23 14:49 +0700

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
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec gcd (a: int64) (b: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int64 = a
        let mutable y: int64 = b
        while y <> (int64 0) do
            let t: int64 = ((x % y + y) % y)
            x <- y
            y <- t
        if x < (int64 0) then
            __ret <- -x
            raise Return
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and lcm (a: int64) (b: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable a = a
    let mutable b = b
    try
        __ret <- (_floordiv64 (int64 a) (int64 (gcd (a) (b)))) * b
        raise Return
        __ret
    with
        | Return -> __ret
and solution (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        if n <= (int64 0) then
            ignore (failwith ("Parameter n must be greater than or equal to one."))
        let mutable result: int64 = int64 1
        let mutable i: int64 = int64 2
        while i <= n do
            result <- lcm (result) (i)
            i <- i + (int64 1)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%A" (solution (int64 10)))
ignore (printfn "%A" (solution (int64 15)))
ignore (printfn "%A" (solution (int64 22)))
ignore (printfn "%A" (solution (int64 20)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
