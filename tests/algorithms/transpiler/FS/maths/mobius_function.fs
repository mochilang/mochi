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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec primeFactors (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable i: int = 2
        let mutable factors: int array = Array.empty<int>
        while ((int64 i) * (int64 i)) <= (int64 n) do
            if (((n % i + i) % i)) = 0 then
                factors <- Array.append factors [|i|]
                n <- _floordiv n i
            else
                i <- i + 1
        if n > 1 then
            factors <- Array.append factors [|n|]
        __ret <- factors
        raise Return
        __ret
    with
        | Return -> __ret
let rec isSquareFree (factors: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable factors = factors
    try
        let mutable seen: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        for f in factors do
            if seen.ContainsKey(f) then
                __ret <- false
                raise Return
            seen.[f] <- true
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec mobius (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable factors: int array = primeFactors (n)
        if isSquareFree (factors) then
            __ret <- if ((((Seq.length (factors)) % 2 + 2) % 2)) = 0 then 1 else (-1)
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (mobius (24))
printfn "%d" (mobius (-1))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
