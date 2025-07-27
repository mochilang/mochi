// Generated 2025-07-27 23:45 +0700

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
let rec binom (n: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable k = k
    try
        if (k < 0) || (k > n) then
            __ret <- 0
            raise Return
        let mutable kk: int = k
        if kk > (n - kk) then
            kk <- n - kk
        let mutable res: int = 1
        let mutable i: int = 0
        while i < kk do
            res <- res * (n - i)
            i <- i + 1
            res <- unbox<int> (res / i)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and catalan (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- unbox<int> ((binom (2 * n) n) / (n + 1))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        for i in 0 .. (15 - 1) do
            printfn "%s" (string (catalan i))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
