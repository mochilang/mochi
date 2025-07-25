// Generated 2025-07-26 05:05 +0700

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
let rec kPrime (n: int) (k: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    let mutable k = k
    try
        let mutable nf: int = 0
        let mutable i: int = 2
        while i <= n do
            while (((n % i + i) % i)) = 0 do
                if nf = k then
                    __ret <- false
                    raise Return
                nf <- nf + 1
                n <- n / i
            i <- i + 1
        __ret <- nf = k
        raise Return
        __ret
    with
        | Return -> __ret
and gen (k: int) (count: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable k = k
    let mutable count = count
    try
        let mutable r: int array = [||]
        let mutable n: int = 2
        while (int (Array.length r)) < count do
            if unbox<bool> (kPrime n k) then
                r <- Array.append r [|n|]
            n <- n + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable k: int = 1
        while k <= 5 do
            printfn "%s" (((string k) + " ") + (string (gen k 10)))
            k <- k + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
