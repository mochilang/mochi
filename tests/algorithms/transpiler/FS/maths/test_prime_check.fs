// Generated 2025-08-08 18:58 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_prime (number: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable number = number
    try
        if number < 0 then
            failwith ("is_prime() only accepts positive integers")
        if number < 2 then
            __ret <- false
            raise Return
        if number < 4 then
            __ret <- true
            raise Return
        if ((((number % 2 + 2) % 2)) = 0) || ((((number % 3 + 3) % 3)) = 0) then
            __ret <- false
            raise Return
        let mutable i: int = 5
        while ((int64 i) * (int64 i)) <= (int64 number) do
            if ((((number % i + i) % i)) = 0) || ((((number % (i + 2) + (i + 2)) % (i + 2))) = 0) then
                __ret <- false
                raise Return
            i <- i + 6
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (is_prime (2)))
printfn "%s" (_str (is_prime (3)))
printfn "%s" (_str (is_prime (5)))
printfn "%s" (_str (is_prime (7)))
printfn "%s" (_str (is_prime (11)))
printfn "%s" (_str (is_prime (13)))
printfn "%s" (_str (is_prime (17)))
printfn "%s" (_str (is_prime (19)))
printfn "%s" (_str (is_prime (23)))
printfn "%s" (_str (is_prime (29)))
printfn "%s" (_str (is_prime (0)))
printfn "%s" (_str (is_prime (1)))
printfn "%s" (_str (is_prime (4)))
printfn "%s" (_str (is_prime (6)))
printfn "%s" (_str (is_prime (9)))
printfn "%s" (_str (is_prime (15)))
printfn "%s" (_str (is_prime (105)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
