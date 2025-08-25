// Generated 2025-08-25 22:27 +0700

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
    match box v with
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec factorial (digit: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable digit = digit
    try
        __ret <- if (digit = (int64 0)) || (digit = (int64 1)) then (int64 1) else (digit * (factorial (digit - (int64 1))))
        raise Return
        __ret
    with
        | Return -> __ret
and is_krishnamurthy (n: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let mutable duplicate: int64 = n
        let mutable fact_sum: int64 = int64 0
        while duplicate > (int64 0) do
            let digit: int64 = ((duplicate % (int64 10) + (int64 10)) % (int64 10))
            fact_sum <- fact_sum + (factorial (int64 digit))
            duplicate <- _floordiv64 (int64 duplicate) (int64 (int64 10))
        __ret <- fact_sum = n
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (is_krishnamurthy (int64 145))))
ignore (printfn "%s" (_str (is_krishnamurthy (int64 240))))
ignore (printfn "%s" (_str (is_krishnamurthy (int64 1))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
