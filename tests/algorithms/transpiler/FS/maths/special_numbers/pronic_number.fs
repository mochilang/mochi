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
let rec int_sqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable r: int = 0
        while ((int64 (r + 1)) * (int64 (r + 1))) <= (int64 n) do
            r <- r + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and is_pronic (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        if n < 0 then
            __ret <- false
            raise Return
        if (((n % 2 + 2) % 2)) <> 0 then
            __ret <- false
            raise Return
        let root: int = int_sqrt (n)
        __ret <- (int64 n) = ((int64 root) * (int64 (root + 1)))
        raise Return
        __ret
    with
        | Return -> __ret
and test_is_pronic () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if is_pronic (-1) then
            failwith ("-1 should not be pronic")
        if not (is_pronic (0)) then
            failwith ("0 should be pronic")
        if not (is_pronic (2)) then
            failwith ("2 should be pronic")
        if is_pronic (5) then
            failwith ("5 should not be pronic")
        if not (is_pronic (6)) then
            failwith ("6 should be pronic")
        if is_pronic (8) then
            failwith ("8 should not be pronic")
        if not (is_pronic (30)) then
            failwith ("30 should be pronic")
        if is_pronic (32) then
            failwith ("32 should not be pronic")
        if not (is_pronic (2147441940)) then
            failwith ("2147441940 should be pronic")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_is_pronic()
        printfn "%b" (is_pronic (56))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
