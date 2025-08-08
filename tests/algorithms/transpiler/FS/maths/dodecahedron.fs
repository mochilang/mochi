// Generated 2025-08-08 17:35 +0700

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
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and abs_val (num: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable num = num
    try
        __ret <- if num < 0.0 then (-num) else num
        raise Return
        __ret
    with
        | Return -> __ret
and approx_equal (a: float) (b: float) (eps: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable eps = eps
    try
        __ret <- (abs_val (a - b)) < eps
        raise Return
        __ret
    with
        | Return -> __ret
and dodecahedron_surface_area (edge: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable edge = edge
    try
        if edge <= 0 then
            failwith ("Length must be a positive.")
        let term: float = sqrtApprox (25.0 + (10.0 * (sqrtApprox (5.0))))
        let e: float = float edge
        __ret <- ((3.0 * term) * e) * e
        raise Return
        __ret
    with
        | Return -> __ret
and dodecahedron_volume (edge: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable edge = edge
    try
        if edge <= 0 then
            failwith ("Length must be a positive.")
        let term: float = (15.0 + (7.0 * (sqrtApprox (5.0)))) / 4.0
        let e: float = float edge
        __ret <- ((term * e) * e) * e
        raise Return
        __ret
    with
        | Return -> __ret
and test_dodecahedron () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if not (approx_equal (dodecahedron_surface_area (5)) (516.1432201766901) (0.0001)) then
            failwith ("surface area 5 failed")
        if not (approx_equal (dodecahedron_surface_area (10)) (2064.5728807067603) (0.0001)) then
            failwith ("surface area 10 failed")
        if not (approx_equal (dodecahedron_volume (5)) (957.8898700780791) (0.0001)) then
            failwith ("volume 5 failed")
        if not (approx_equal (dodecahedron_volume (10)) (7663.118960624633) (0.0001)) then
            failwith ("volume 10 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_dodecahedron()
        printfn "%g" (dodecahedron_surface_area (5))
        printfn "%g" (dodecahedron_volume (5))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
