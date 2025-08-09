// Generated 2025-08-09 15:58 +0700

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
type FibPair = {
    mutable _fn: int
    mutable _fn1: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec _fib (n: int) =
    let mutable __ret : FibPair = Unchecked.defaultof<FibPair>
    let mutable n = n
    try
        if n = 0 then
            __ret <- { _fn = 0; _fn1 = 1 }
            raise Return
        let half: FibPair = _fib (_floordiv n 2)
        let a: int = half._fn
        let b: int = half._fn1
        let c: int64 = (int64 a) * (((int64 b) * (int64 2)) - (int64 a))
        let d: int64 = ((int64 a) * (int64 a)) + ((int64 b) * (int64 b))
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- { _fn = int c; _fn1 = int d }
            raise Return
        __ret <- { _fn = int d; _fn1 = int (c + d) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec fibonacci (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            failwith ("Negative arguments are not supported")
        let res: FibPair = _fib (n)
        __ret <- res._fn
        raise Return
        __ret
    with
        | Return -> __ret
let mutable i: int = 0
while i < 13 do
    printfn "%s" (_str (fibonacci (i)))
    i <- i + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
