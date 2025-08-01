// Generated 2025-08-02 00:26 +0700

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
let xs: float array = [|-0.5; 0.1; 0.5; 1.0; 1.5; 2.0; 3.0; 10.0; 140.0; 170.0|]
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
and expf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 20 do
            term <- float ((term * x) / (float (float i)))
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and powf (``base``: float) (exp: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        __ret <- expf (exp * (float (ln ``base``)))
        raise Return
        __ret
    with
        | Return -> __ret
and lanczos7 (z: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable z = z
    try
        let t: float = z + 6.5
        let x: float = (((((((0.9999999999998099 + (676.5203681218851 / z)) - (1259.1392167224028 / (z + 1.0))) + (771.3234287776531 / (z + 2.0))) - (176.6150291621406 / (z + 3.0))) + (12.507343278686905 / (z + 4.0))) - (0.13857109526572012 / (z + 5.0))) + (0.000009984369578019572 / (z + 6.0))) + (0.00000015056327351493116 / (z + 7.0))
        __ret <- (float ((2.5066282746310002 * (float (powf t (z - 0.5)))) * (powf 2.718281828459045 (-t)))) * x
        raise Return
        __ret
    with
        | Return -> __ret
for x in xs do
    printfn "%s" (((string x) + " ") + (string (lanczos7 x)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
