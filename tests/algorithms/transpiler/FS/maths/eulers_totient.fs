// Generated 2025-08-08 17:35 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec totient (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable is_prime: bool array = Array.empty<bool>
        let mutable totients: int array = Array.empty<int>
        let mutable primes: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= n do
            is_prime <- Array.append is_prime [|true|]
            totients <- Array.append totients [|(i - 1)|]
            i <- i + 1
        i <- 2
        try
            while i <= n do
                try
                    if _idx is_prime (i) then
                        primes <- Array.append primes [|i|]
                    let mutable j: int = 0
                    try
                        while j < (Seq.length (primes)) do
                            try
                                let p: int = _idx primes (j)
                                if (i * p) >= n then
                                    raise Break
                                is_prime.[i * p] <- false
                                if (((i % p + p) % p)) = 0 then
                                    totients.[i * p] <- (_idx totients (i)) * p
                                    raise Break
                                totients.[i * p] <- (_idx totients (i)) * (p - 1)
                                j <- j + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    i <- i + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- totients
        raise Return
        __ret
    with
        | Return -> __ret
and test_totient () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let expected: int array = unbox<int array> [|-1; 0; 1; 2; 2; 4; 2; 6; 4; 6; 9|]
        let res: int array = totient (10)
        let mutable idx: int = 0
        while idx < (Seq.length (expected)) do
            if (_idx res (idx)) <> (_idx expected (idx)) then
                failwith ("totient mismatch at " + (_str (idx)))
            idx <- idx + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_totient()
        let n: int = 10
        let res: int array = totient (n)
        let mutable i: int = 1
        while i < n do
            printfn "%s" ((((_str (i)) + " has ") + (_str (_idx res (i)))) + " relative primes.")
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
