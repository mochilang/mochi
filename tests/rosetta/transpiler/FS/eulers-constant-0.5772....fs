// Generated 2025-08-03 21:12 +0700

exception Return
let mutable __ret = ()

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
let rec ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable k: float = 0.0
        let mutable v: float = x
        while v >= 2.0 do
            v <- v / 2.0
            k <- k + 1.0
        while v < 1.0 do
            v <- v * 2.0
            k <- k - 1.0
        let z: float = (v - 1.0) / (v + 1.0)
        let mutable zpow: float = z
        let mutable sum: float = z
        let mutable i: int = 3
        while i <= 9 do
            zpow <- (zpow * z) * z
            sum <- sum + (zpow / (float i))
            i <- i + 2
        let ln2: float = 0.6931471805599453
        __ret <- (k * ln2) + (2.0 * sum)
        raise Return
        __ret
    with
        | Return -> __ret
and harmonic (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable sum: float = 0.0
        let mutable i: int = 1
        while i <= n do
            sum <- sum + (1.0 / (float i))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let n: int = 100000
        let gamma: float = (harmonic (n)) - (ln (float n))
        printfn "%s" (string (gamma))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
