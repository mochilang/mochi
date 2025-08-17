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
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec factorial (digit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable digit = digit
    try
        __ret <- if (digit = 0) || (digit = 1) then 1 else (digit * (factorial (digit - 1)))
        raise Return
        __ret
    with
        | Return -> __ret
and is_krishnamurthy (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let mutable duplicate: int = n
        let mutable fact_sum: int = 0
        while duplicate > 0 do
            let digit: int = ((duplicate % 10 + 10) % 10)
            fact_sum <- fact_sum + (factorial (digit))
            duplicate <- _floordiv (int duplicate) (int 10)
        __ret <- fact_sum = n
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (is_krishnamurthy (145))))
ignore (printfn "%s" (_str (is_krishnamurthy (240))))
ignore (printfn "%s" (_str (is_krishnamurthy (1))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
