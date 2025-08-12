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

let NUM_PRIMES: int = 100
let rec generate_primes (limit: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable limit = limit
    try
        let mutable is_prime: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i <= limit do
            is_prime <- Array.append is_prime [|true|]
            i <- i + 1
        is_prime.[0] <- false
        is_prime.[1] <- false
        i <- 2
        while (i * i) <= limit do
            if _idx is_prime (int i) then
                let mutable j: int = i * i
                while j <= limit do
                    is_prime.[j] <- false
                    j <- j + i
            i <- i + 1
        let mutable primes: int array = Array.empty<int>
        i <- 2
        while i <= limit do
            if _idx is_prime (int i) then
                primes <- Array.append primes [|i|]
            i <- i + 1
        __ret <- primes
        raise Return
        __ret
    with
        | Return -> __ret
let mutable primes: int array = generate_primes (NUM_PRIMES)
let rec contains (xs: int array) (value: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    let mutable value = value
    try
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) = value then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let mutable partition_cache: System.Collections.Generic.IDictionary<int, int array> = _dictCreate []
let rec partition (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 0 then
            __ret <- Array.empty<int>
            raise Return
        if n = 0 then
            __ret <- unbox<int array> [|1|]
            raise Return
        if partition_cache.ContainsKey(n) then
            __ret <- _dictGet partition_cache (n)
            raise Return
        let mutable ret: int array = Array.empty<int>
        try
            for prime in primes do
                try
                    if prime > n then
                        raise Continue
                    let subs: int array = partition (n - prime)
                    for sub in subs do
                        let prod: int = sub * prime
                        if not (contains (ret) (prod)) then
                            ret <- Array.append ret [|prod|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        partition_cache <- _dictAdd (partition_cache) (n) (ret)
        __ret <- ret
        raise Return
        __ret
    with
        | Return -> __ret
and solution (threshold: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable threshold = threshold
    try
        let mutable number_to_partition: int = 1
        while number_to_partition < NUM_PRIMES do
            let parts: int array = partition (number_to_partition)
            if (Seq.length (parts)) > threshold then
                __ret <- number_to_partition
                raise Return
            number_to_partition <- number_to_partition + 1
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let result: int = solution (5000)
ignore (printfn "%s" ("solution() = " + (_str (result))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
