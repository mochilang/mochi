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
type Point = {
    mutable _x: float
    mutable _y: float
}
let PI: float = 3.141592653589793
let mutable _seed: int = 1
let rec next_seed (_x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _x = _x
    try
        __ret <- int ((((((int64 _x) * (int64 1103515245)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        raise Return
        __ret
    with
        | Return -> __ret
and rand_unit () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        _seed <- next_seed (_seed)
        __ret <- (float _seed) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and is_in_unit_circle (p: Point) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p = p
    try
        __ret <- (((p._x) * (p._x)) + ((p._y) * (p._y))) <= 1.0
        raise Return
        __ret
    with
        | Return -> __ret
and random_unit_square () =
    let mutable __ret : Point = Unchecked.defaultof<Point>
    try
        __ret <- { _x = rand_unit(); _y = rand_unit() }
        raise Return
        __ret
    with
        | Return -> __ret
and estimate_pi (simulations: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable simulations = simulations
    try
        if simulations < 1 then
            failwith ("At least one simulation is necessary to estimate PI.")
        let mutable inside: int = 0
        let mutable i: int = 0
        while i < simulations do
            let p: Point = random_unit_square()
            if is_in_unit_circle (p) then
                inside <- inside + 1
            i <- i + 1
        __ret <- (4.0 * (float inside)) / (float simulations)
        raise Return
        __ret
    with
        | Return -> __ret
and abs_float (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        __ret <- if _x < 0.0 then (-_x) else _x
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let n: int = 10000
        let my_pi: float = estimate_pi (n)
        let error: float = abs_float (my_pi - PI)
        printfn "%s" ((("An estimate of PI is " + (_str (my_pi))) + " with an error of ") + (_str (error)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
