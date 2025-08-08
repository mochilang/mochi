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
let rec is_prime (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 2 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) = 0 then
            __ret <- n = 2
            raise Return
        let mutable i: int = 3
        while ((int64 i) * (int64 i)) <= (int64 n) do
            if (((n % i + i) % i)) = 0 then
                __ret <- false
                raise Return
            i <- i + 2
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and twin_prime (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        __ret <- if (is_prime (number)) && (is_prime (number + 2)) then (number + 2) else (-1)
        raise Return
        __ret
    with
        | Return -> __ret
and test_twin_prime () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (twin_prime (3)) <> 5 then
            failwith ("twin_prime(3) failed")
        if (twin_prime (4)) <> (-1) then
            failwith ("twin_prime(4) failed")
        if (twin_prime (5)) <> 7 then
            failwith ("twin_prime(5) failed")
        if (twin_prime (17)) <> 19 then
            failwith ("twin_prime(17) failed")
        if (twin_prime (0)) <> (-1) then
            failwith ("twin_prime(0) failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_twin_prime()
        printfn "%d" (twin_prime (3))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
