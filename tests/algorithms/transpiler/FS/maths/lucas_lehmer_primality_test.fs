// Generated 2025-08-12 07:47 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec pow2 (p: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable p = p
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < p do
            result <- int ((int64 result) * (int64 2))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and lucas_lehmer_test (p: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p = p
    try
        if p < 2 then
            failwith ("p should not be less than 2!")
        if p = 2 then
            __ret <- true
            raise Return
        let mutable s: int = 4
        let m: int = (pow2 (p)) - 1
        let mutable i: int = 0
        while i < (p - 2) do
            s <- int ((((((int64 s) * (int64 s)) - (int64 2)) % (int64 m) + (int64 m)) % (int64 m)))
            i <- i + 1
        __ret <- s = 0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (lucas_lehmer_test (7)))
        printfn "%s" (_str (lucas_lehmer_test (11)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
