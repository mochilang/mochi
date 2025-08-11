// Generated 2025-08-11 16:20 +0700

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
let rec next_number (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        let mutable n: int = number
        let mutable total: int = 0
        while n > 0 do
            let d: int = ((n % 10 + 10) % 10)
            total <- int ((int64 total) + ((int64 d) * (int64 d)))
            n <- _floordiv n 10
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec chain (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        let mutable n: int = number
        while (n <> 1) && (n <> 89) do
            n <- next_number (n)
        __ret <- n = 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec solution (limit: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable limit = limit
    try
        let mutable count: int = 0
        let mutable i: int = 1
        while i < limit do
            if not (chain (i)) then
                count <- count + 1
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (next_number (44)))
printfn "%s" (_str (next_number (10)))
printfn "%s" (_str (next_number (32)))
printfn "%s" (_str (chain (10)))
printfn "%s" (_str (chain (58)))
printfn "%s" (_str (chain (1)))
printfn "%s" (_str (solution (100)))
printfn "%s" (_str (solution (1000)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
