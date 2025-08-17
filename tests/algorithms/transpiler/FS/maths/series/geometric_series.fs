// Generated 2025-08-17 12:28 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec geometric_series (nth_term: float) (start_term_a: float) (common_ratio_r: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable nth_term = nth_term
    let mutable start_term_a = start_term_a
    let mutable common_ratio_r = common_ratio_r
    try
        let n: int = int nth_term
        if ((n <= 0) || (start_term_a = 0.0)) || (common_ratio_r = 0.0) then
            __ret <- Array.empty<float>
            raise Return
        let mutable series: float array = Array.empty<float>
        let mutable current: float = start_term_a
        let mutable i: int = 0
        while i < n do
            series <- Array.append series [|current|]
            current <- current * common_ratio_r
            i <- i + 1
        __ret <- series
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_repr (geometric_series (4.0) (2.0) (2.0))))
ignore (printfn "%s" (_repr (geometric_series (4.0) (2.0) (-2.0))))
ignore (printfn "%s" (_repr (geometric_series (4.0) (-2.0) (2.0))))
ignore (printfn "%s" (_repr (geometric_series (-4.0) (2.0) (2.0))))
ignore (printfn "%s" (_repr (geometric_series (0.0) (100.0) (500.0))))
ignore (printfn "%s" (_repr (geometric_series (1.0) (1.0) (1.0))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
