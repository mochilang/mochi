// Generated 2025-08-08 18:09 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let rec _str v =
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
let rec perfect_square (num: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    try
        if num < 0 then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while ((int64 i) * (int64 i)) <= (int64 num) do
            if ((int64 i) * (int64 i)) = (int64 num) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec perfect_square_binary_search (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 0 then
            __ret <- false
            raise Return
        let mutable left: int = 0
        let mutable right: int = n
        while left <= right do
            let mid: int = _floordiv (left + right) 2
            let sq: int64 = (int64 mid) * (int64 mid)
            if sq = (int64 n) then
                __ret <- true
                raise Return
            if sq > (int64 n) then
                right <- mid - 1
            else
                left <- mid + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (perfect_square (9)))
printfn "%s" (_str (perfect_square (10)))
printfn "%s" (_str (perfect_square_binary_search (16)))
printfn "%s" (_str (perfect_square_binary_search (10)))
printfn "%s" (_str (perfect_square_binary_search (-1)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
