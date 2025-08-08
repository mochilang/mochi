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
let rec pow_float (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable exponent: int = exp
        if exponent < 0 then
            exponent <- -exponent
            let mutable i: int = 0
            while i < exponent do
                result <- result * ``base``
                i <- i + 1
            __ret <- 1.0 / result
            raise Return
        let mutable i: int = 0
        while i < exponent do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_geometric_progression (first_term: int) (common_ratio: int) (num_of_terms: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable first_term = first_term
    let mutable common_ratio = common_ratio
    let mutable num_of_terms = num_of_terms
    try
        if common_ratio = 1 then
            __ret <- float ((int64 num_of_terms) * (int64 first_term))
            raise Return
        let a: float = float first_term
        let r: float = float common_ratio
        __ret <- (a / (1.0 - r)) * (1.0 - (pow_float (r) (num_of_terms)))
        raise Return
        __ret
    with
        | Return -> __ret
and test_sum () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (sum_of_geometric_progression (1) (2) (10)) <> 1023.0 then
            failwith ("example1 failed")
        if (sum_of_geometric_progression (1) (10) (5)) <> 11111.0 then
            failwith ("example2 failed")
        if (sum_of_geometric_progression (-1) (2) (10)) <> (-1023.0) then
            failwith ("example3 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_sum()
        printfn "%g" (sum_of_geometric_progression (1) (2) (10))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
