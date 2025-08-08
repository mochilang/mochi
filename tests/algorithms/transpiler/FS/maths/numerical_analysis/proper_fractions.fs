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
let rec gcd (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        while y <> 0 do
            let t: int = ((x % y + y) % y)
            x <- y
            y <- t
        if x < 0 then
            __ret <- -x
            raise Return
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and proper_fractions (den: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable den = den
    try
        if den < 0 then
            failwith ("The Denominator Cannot be less than 0")
        let mutable res: string array = Array.empty<string>
        let mutable n: int = 1
        while n < den do
            if (gcd (n) (den)) = 1 then
                res <- Array.append res [|(((_str (n)) + "/") + (_str (den)))|]
            n <- n + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and test_proper_fractions () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let a: string array = proper_fractions (10)
        if a <> [|"1/10"; "3/10"; "7/10"; "9/10"|] then
            failwith ("test 10 failed")
        let b: string array = proper_fractions (5)
        if b <> [|"1/5"; "2/5"; "3/5"; "4/5"|] then
            failwith ("test 5 failed")
        let c: string array = proper_fractions (0)
        if c <> [||] then
            failwith ("test 0 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_proper_fractions()
        printfn "%s" (_str (proper_fractions (10)))
        printfn "%s" (_str (proper_fractions (5)))
        printfn "%s" (_str (proper_fractions (0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
