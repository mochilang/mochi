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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec odd_sieve (num: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable num = num
    try
        if num <= 2 then
            __ret <- Array.empty<int>
            raise Return
        if num = 3 then
            __ret <- unbox<int array> [|2|]
            raise Return
        let size: int = (_floordiv num 2) - 1
        let mutable sieve: bool array = Array.empty<bool>
        let mutable idx: int = 0
        while idx < size do
            sieve <- Array.append sieve [|true|]
            idx <- idx + 1
        let mutable i: int = 3
        while ((int64 i) * (int64 i)) <= (int64 num) do
            let s_idx: int = (_floordiv i 2) - 1
            if _idx sieve (int s_idx) then
                let mutable j: int64 = (int64 i) * (int64 i)
                while j < (int64 num) do
                    let j_idx: int64 = (j / (int64 2)) - (int64 1)
                    sieve.[int j_idx] <- false
                    j <- j + ((int64 2) * (int64 i))
            i <- i + 2
        let mutable primes: int array = unbox<int array> [|2|]
        let mutable n: int = 3
        let mutable k: int = 0
        while n < num do
            if _idx sieve (int k) then
                primes <- Array.append primes [|n|]
            n <- n + 2
            k <- k + 1
        __ret <- primes
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (odd_sieve (2)))
printfn "%s" (_repr (odd_sieve (3)))
printfn "%s" (_repr (odd_sieve (10)))
printfn "%s" (_repr (odd_sieve (20)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
