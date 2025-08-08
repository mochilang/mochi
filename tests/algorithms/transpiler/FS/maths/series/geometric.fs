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
let rec is_geometric_series (series: float array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable series = series
    try
        if (Seq.length (series)) = 0 then
            failwith ("Input list must be a non empty list")
        if (Seq.length (series)) = 1 then
            __ret <- true
            raise Return
        if (_idx series (0)) = 0.0 then
            __ret <- false
            raise Return
        let ratio: float = (_idx series (1)) / (_idx series (0))
        let mutable i: int = 0
        while i < ((Seq.length (series)) - 1) do
            if (_idx series (i)) = 0.0 then
                __ret <- false
                raise Return
            if ((_idx series (i + 1)) / (_idx series (i))) <> ratio then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and geometric_mean (series: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable series = series
    try
        if (Seq.length (series)) = 0 then
            failwith ("Input list must be a non empty list")
        let mutable product: float = 1.0
        let mutable i: int = 0
        while i < (Seq.length (series)) do
            product <- product * (_idx series (i))
            i <- i + 1
        let n: int = Seq.length (series)
        __ret <- nth_root (product) (n)
        raise Return
        __ret
    with
        | Return -> __ret
and pow_float (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and nth_root (value: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable value = value
    let mutable n = n
    try
        if value = 0.0 then
            __ret <- 0.0
            raise Return
        let mutable low: float = 0.0
        let mutable high: float = value
        if value < 1.0 then
            high <- 1.0
        let mutable mid: float = (low + high) / 2.0
        let mutable i: int = 0
        while i < 40 do
            let mp: float = pow_float (mid) (n)
            if mp > value then
                high <- mid
            else
                low <- mid
            mid <- (low + high) / 2.0
            i <- i + 1
        __ret <- mid
        raise Return
        __ret
    with
        | Return -> __ret
and test_geometric () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let a: float array = unbox<float array> [|2.0; 4.0; 8.0|]
        if not (is_geometric_series (a)) then
            failwith ("expected geometric series")
        let b: float array = unbox<float array> [|1.0; 2.0; 3.0|]
        if is_geometric_series (b) then
            failwith ("expected non geometric series")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_geometric()
        printfn "%g" (geometric_mean (unbox<float array> [|2.0; 4.0; 8.0|]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
