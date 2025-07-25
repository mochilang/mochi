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
let rec fib (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 2 then
            __ret <- n
            raise Return
        let mutable a: int = 0
        let mutable b: int = 1
        let mutable i: int = 1
        while i < n do
            let t: int = a + b
            a <- b
            b <- t
            i <- i + 1
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        for i in [|-1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10|] do
            if (int i) < 0 then
                printfn "%s" (("fib(" + (string i)) + ") returned error: negative n is forbidden")
            else
                printfn "%s" ((("fib(" + (string i)) + ") = ") + (string (fib (int i))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
