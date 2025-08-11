// Generated 2025-08-09 23:14 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sum_of_multiples (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable total: int = 0
        let mutable terms: int = _floordiv (n - 1) 3
        total <- int ((int64 total) + (((int64 terms) * ((int64 6) + ((int64 (terms - 1)) * (int64 3)))) / (int64 2)))
        terms <- _floordiv (n - 1) 5
        total <- int ((int64 total) + (((int64 terms) * ((int64 10) + ((int64 (terms - 1)) * (int64 5)))) / (int64 2)))
        terms <- _floordiv (n - 1) 15
        total <- int ((int64 total) - (((int64 terms) * ((int64 30) + ((int64 (terms - 1)) * (int64 15)))) / (int64 2)))
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("solution() = " + (_str (sum_of_multiples (1000))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
