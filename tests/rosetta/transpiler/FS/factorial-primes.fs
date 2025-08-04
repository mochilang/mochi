// Generated 2025-08-04 20:44 +0700

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
let rec factorial (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable result: int = 1
        let mutable i: int = 2
        while i <= n do
            result <- result * i
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and isPrime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        let mutable d: int = 3
        while (d * d) <= n do
            if (((n % d + d) % d)) = 0 then
                __ret <- false
                raise Return
            d <- d + 2
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable out: string = s
        while (String.length (out)) < w do
            out <- " " + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable n: int = 0
        let mutable count: int = 0
        while count < 10 do
            n <- n + 1
            let f: int = factorial (n)
            if isPrime (f - 1) then
                count <- count + 1
                printfn "%s" (((((padLeft (string (count)) (2)) + ": ") + (padLeft (string (n)) (2))) + "! - 1 = ") + (string (f - 1)))
            if (count < 10) && (isPrime (f + 1)) then
                count <- count + 1
                printfn "%s" (((((padLeft (string (count)) (2)) + ": ") + (padLeft (string (n)) (2))) + "! + 1 = ") + (string (f + 1)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
