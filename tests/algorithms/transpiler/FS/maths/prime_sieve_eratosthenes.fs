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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec prime_sieve_eratosthenes (num: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable num = num
    try
        if num <= 0 then
            failwith ("Input must be a positive integer")
        let mutable primes: bool array = Array.empty<bool>
        let mutable i: int = 0
        while i <= num do
            primes <- Array.append primes [|true|]
            i <- i + 1
        let mutable p: int = 2
        while ((int64 p) * (int64 p)) <= (int64 num) do
            if _idx primes (p) then
                let mutable j: int64 = (int64 p) * (int64 p)
                while j <= (int64 num) do
                    primes.[int j] <- false
                    j <- j + (int64 p)
            p <- p + 1
        let mutable result: int array = Array.empty<int>
        let mutable k: int = 2
        while k <= num do
            if _idx primes (k) then
                result <- Array.append result [|k|]
            k <- k + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and list_eq (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if (_idx a (i)) <> (_idx b (i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and test_prime_sieve_eratosthenes () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if not (list_eq (prime_sieve_eratosthenes (10)) (unbox<int array> [|2; 3; 5; 7|])) then
            failwith ("test 10 failed")
        if not (list_eq (prime_sieve_eratosthenes (20)) (unbox<int array> [|2; 3; 5; 7; 11; 13; 17; 19|])) then
            failwith ("test 20 failed")
        if not (list_eq (prime_sieve_eratosthenes (2)) (unbox<int array> [|2|])) then
            failwith ("test 2 failed")
        if (Seq.length (prime_sieve_eratosthenes (1))) <> 0 then
            failwith ("test 1 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_prime_sieve_eratosthenes()
        printfn "%s" (_str (prime_sieve_eratosthenes (20)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
