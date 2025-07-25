// Generated 2025-07-25 09:57 +0000

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
let PI: float = 3.141592653589793
let rec sinApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable n: int = 1
        while n <= 12 do
            let denom: float = float ((2 * n) * ((2 * n) + 1))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let dt: float = 0.01
let mutable s: float = 0.0
let mutable t1: float = 0.0
let mutable k1: float = sinApprox 0.0
let mutable i: int = 1
while i <= 200 do
    let t2: float = (float i) * dt
    let k2: float = sinApprox (t2 * PI)
    s <- s + (((k1 + k2) * 0.5) * (t2 - t1))
    t1 <- t2
    k1 <- k2
    i <- i + 1
let mutable i2: int = 1
while i2 <= 50 do
    let t2: float = 2.0 + ((float i2) * dt)
    let k2: float = 0.0
    s <- s + (((k1 + k2) * 0.5) * (t2 - t1))
    t1 <- t2
    k1 <- k2
    i2 <- i2 + 1
printfn "%.1f" s
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
