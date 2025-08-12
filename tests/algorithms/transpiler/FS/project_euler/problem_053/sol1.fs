// Generated 2025-08-12 13:41 +0700

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
let rec combination_exceeds (n: int) (r: int) (limit: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    let mutable r = r
    let mutable limit = limit
    try
        let mutable r2: int = r
        if r2 > (n - r2) then
            r2 <- n - r2
        let mutable result: int = 1
        let mutable k: int = 1
        while k <= r2 do
            result <- _floordiv (result * ((n - r2) + k)) k
            if result > limit then
                __ret <- true
                raise Return
            k <- k + 1
        __ret <- result > limit
        raise Return
        __ret
    with
        | Return -> __ret
and count_exceeding (limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable limit = limit
    try
        let mutable total: int = 0
        let mutable n: int = 1
        while n <= 100 do
            let mutable r: int = 1
            while r <= n do
                if combination_exceeds (n) (r) (limit) then
                    total <- total + 1
                r <- r + 1
            n <- n + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (count_exceeding (1000000))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
