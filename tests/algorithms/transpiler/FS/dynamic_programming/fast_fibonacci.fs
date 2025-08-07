// Generated 2025-08-07 15:46 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type FibPair = {
    fn: int
    fn1: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec _fib (n: int) =
    let mutable __ret : FibPair = Unchecked.defaultof<FibPair>
    let mutable n = n
    try
        if n = 0 then
            __ret <- { fn = 0; fn1 = 1 }
            raise Return
        let half: FibPair = _fib (n / 2)
        let a: int = half.fn
        let b: int = half.fn1
        let c: int = a * ((b * 2) - a)
        let d: int = (a * a) + (b * b)
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- { fn = c; fn1 = d }
            raise Return
        __ret <- { fn = d; fn1 = c + d }
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
        __ret <- res.fn
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
