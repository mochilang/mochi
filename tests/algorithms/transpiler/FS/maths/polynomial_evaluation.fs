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
let rec pow_float (``base``: float) (exponent: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    try
        let mutable exp: int = exponent
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
and evaluate_poly (poly: float array) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable poly = poly
    let mutable x = x
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (poly)) do
            total <- total + ((_idx poly (i)) * (pow_float (x) (i)))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and horner (poly: float array) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable poly = poly
    let mutable x = x
    try
        let mutable result: float = 0.0
        let mutable i: int = (Seq.length (poly)) - 1
        while i >= 0 do
            result <- (result * x) + (_idx poly (i))
            i <- i - 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and test_polynomial_evaluation () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let poly: float array = unbox<float array> [|0.0; 0.0; 5.0; 9.3; 7.0|]
        let x: float = 10.0
        if (evaluate_poly (poly) (x)) <> 79800.0 then
            failwith ("evaluate_poly failed")
        if (horner (poly) (x)) <> 79800.0 then
            failwith ("horner failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_polynomial_evaluation()
        let poly: float array = unbox<float array> [|0.0; 0.0; 5.0; 9.3; 7.0|]
        let x: float = 10.0
        printfn "%g" (evaluate_poly (poly) (x))
        printfn "%g" (horner (poly) (x))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
