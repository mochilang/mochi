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
open System.Collections.Generic

let DIGIT_FACTORIALS: int array = unbox<int array> [|1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880|]
let mutable cache_sum_digit_factorials: System.Collections.Generic.IDictionary<int, int> = unbox<System.Collections.Generic.IDictionary<int, int>> (_dictCreate [(145, 145)])
let mutable chain_length_cache: System.Collections.Generic.IDictionary<int, int> = unbox<System.Collections.Generic.IDictionary<int, int>> (_dictCreate [(145, 0); (169, 3); (36301, 3); (1454, 3); (871, 2); (45361, 2); (872, 2)])
let rec sum_digit_factorials (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if cache_sum_digit_factorials.ContainsKey(n) then
            __ret <- _dictGet cache_sum_digit_factorials (n)
            raise Return
        let mutable m: int = n
        let mutable ret: int = 0
        if m = 0 then
            ret <- _idx DIGIT_FACTORIALS (int 0)
        while m > 0 do
            let digit: int = ((m % 10 + 10) % 10)
            ret <- ret + (_idx DIGIT_FACTORIALS (int digit))
            m <- _floordiv m 10
        cache_sum_digit_factorials <- _dictAdd (cache_sum_digit_factorials) (n) (ret)
        __ret <- ret
        raise Return
        __ret
    with
        | Return -> __ret
and chain_length (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if chain_length_cache.ContainsKey(n) then
            __ret <- _dictGet chain_length_cache (n)
            raise Return
        let mutable chain: int array = Array.empty<int>
        let mutable seen: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
        let mutable current: int = n
        while true do
            if chain_length_cache.ContainsKey(current) then
                let known: int = _dictGet chain_length_cache (current)
                let mutable total: int = known
                let mutable i: int = (Seq.length (chain)) - 1
                while i >= 0 do
                    total <- total + 1
                    chain_length_cache <- _dictAdd (chain_length_cache) (_idx chain (int i)) (total)
                    i <- i - 1
                __ret <- _dictGet chain_length_cache (n)
                raise Return
            if seen.ContainsKey(current) then
                let loop_start: int = _dictGet seen (current)
                let loop_len: int = (Seq.length (chain)) - loop_start
                let mutable i: int = (Seq.length (chain)) - 1
                let mutable ahead: int = 0
                while i >= 0 do
                    if i >= loop_start then
                        chain_length_cache <- _dictAdd (chain_length_cache) (_idx chain (int i)) (loop_len)
                    else
                        chain_length_cache <- _dictAdd (chain_length_cache) (_idx chain (int i)) (loop_len + (ahead + 1))
                    ahead <- ahead + 1
                    i <- i - 1
                __ret <- _dictGet chain_length_cache (n)
                raise Return
            seen <- _dictAdd (seen) (current) (Seq.length (chain))
            chain <- Array.append chain [|current|]
            current <- sum_digit_factorials (current)
        __ret
    with
        | Return -> __ret
and solution (num_terms: int) (max_start: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num_terms = num_terms
    let mutable max_start = max_start
    try
        let mutable count: int = 0
        let mutable i: int = 1
        while i < max_start do
            if (chain_length (i)) = num_terms then
                count <- count + 1
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" ("solution() = " + (_str (solution (60) (1000)))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
