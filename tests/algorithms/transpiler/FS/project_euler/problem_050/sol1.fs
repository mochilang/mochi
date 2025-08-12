// Generated 2025-08-12 13:41 +0700

exception Break
exception Continue

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec prime_sieve (limit: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable limit = limit
    try
        if limit <= 2 then
            __ret <- Array.empty<int>
            raise Return
        let mutable is_prime: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i < limit do
            is_prime <- Array.append is_prime [|true|]
            i <- i + 1
        is_prime.[0] <- false
        is_prime.[1] <- false
        let mutable p: int = 3
        while (p * p) < limit do
            let mutable index: int = p * 2
            while index < limit do
                is_prime.[index] <- false
                index <- index + p
            p <- p + 2
        let mutable primes: int array = unbox<int array> [|2|]
        let mutable n: int = 3
        while n < limit do
            if _idx is_prime (int n) then
                primes <- Array.append primes [|n|]
            n <- n + 2
        __ret <- primes
        raise Return
        __ret
    with
        | Return -> __ret
and solution (ceiling: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ceiling = ceiling
    try
        let mutable primes: int array = prime_sieve (ceiling)
        let mutable prime_map: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let mutable i: int = 0
        while i < (Seq.length (primes)) do
            prime_map <- _dictAdd (prime_map) (_idx primes (int i)) (true)
            i <- i + 1
        let mutable prefix: int array = unbox<int array> [|0|]
        i <- 0
        while i < (Seq.length (primes)) do
            prefix <- Array.append prefix [|((_idx prefix (int i)) + (_idx primes (int i)))|]
            i <- i + 1
        let mutable max_len: int = 0
        while (max_len < (Seq.length (prefix))) && ((_idx prefix (int max_len)) < ceiling) do
            max_len <- max_len + 1
        let mutable L: int = max_len
        try
            while L > 0 do
                try
                    let mutable start: int = 0
                    try
                        while (start + L) <= (Seq.length (primes)) do
                            try
                                let s: int = (_idx prefix (int (start + L))) - (_idx prefix (int start))
                                if s >= ceiling then
                                    raise Break
                                if _dictGet prime_map (s) then
                                    __ret <- s
                                    raise Return
                                start <- start + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    L <- L - 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let ans: int = solution (1000000)
ignore (printfn "%s" ("solution() = " + (_str (ans))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
