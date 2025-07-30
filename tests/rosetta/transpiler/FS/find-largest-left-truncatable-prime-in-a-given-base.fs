// Generated 2025-07-30 21:05 +0700

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
let rec isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        let mutable i: int = 2
        while (i * i) <= n do
            if (((n % i + i) % i)) = 0 then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and search (``base``: int) (prefix: int) (depth: int) (limit: int) (best: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable prefix = prefix
    let mutable depth = depth
    let mutable limit = limit
    let mutable best = best
    try
        let mutable b: int = best
        let mutable d: int = 1
        while d < ``base`` do
            let ``val``: int = (prefix * ``base``) + d
            if isPrime ``val`` then
                if ``val`` > b then
                    b <- ``val``
                if (depth + 1) < limit then
                    b <- search ``base`` ``val`` (depth + 1) limit b
            d <- d + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and largest (``base``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    try
        __ret <- search ``base`` 0 0 6 0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable b: int = 3
        while b <= 17 do
            printfn "%s" (((string b) + ": ") + (string (largest b)))
            b <- b + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
