// Generated 2025-07-26 04:38 +0700

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
let rec powf (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and nthRoot (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let mutable low: float = 0.0
        let mutable high: float = x
        let mutable i: int = 0
        while i < 60 do
            let mid: float = (low + high) / 2.0
            if (float (powf mid n)) > x then
                high <- mid
            else
                low <- mid
            i <- i + 1
        __ret <- low
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable sum: float = 0.0
        let mutable sumRecip: float = 0.0
        let mutable prod: float = 1.0
        let mutable n: int = 1
        while n <= 10 do
            let f: float = float n
            sum <- sum + f
            sumRecip <- sumRecip + (1.0 / f)
            prod <- prod * f
            n <- n + 1
        let count: float = 10.0
        let a: float = sum / count
        let g: float = nthRoot prod 10
        let h: float = count / sumRecip
        printfn "%s" ((((("A: " + (string a)) + " G: ") + (string g)) + " H: ") + (string h))
        printfn "%s" ("A >= G >= H: " + (string ((a >= g) && (g >= h))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
