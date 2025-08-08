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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Point = {
    mutable _x: float
    mutable _y: float
    mutable _z: float
}
let rec absf (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        __ret <- if _x < 0.0 then (-_x) else _x
        raise Return
        __ret
    with
        | Return -> __ret
and sqrt_approx (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        if _x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = _x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (_x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and distance (a: Point) (b: Point) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let dx: float = (b._x) - (a._x)
        let dy: float = (b._y) - (a._y)
        let dz: float = (b._z) - (a._z)
        __ret <- sqrt_approx (absf (((dx * dx) + (dy * dy)) + (dz * dz)))
        raise Return
        __ret
    with
        | Return -> __ret
and point_to_string (p: Point) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        __ret <- ((((("Point(" + (_str (p._x))) + ", ") + (_str (p._y))) + ", ") + (_str (p._z))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
and test_distance () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let p1: Point = { _x = 2.0; _y = -1.0; _z = 7.0 }
        let p2: Point = { _x = 1.0; _y = -3.0; _z = 5.0 }
        let d: float = distance (p1) (p2)
        if (absf (d - 3.0)) > 0.0001 then
            failwith ("distance test failed")
        printfn "%s" ((((("Distance from " + (point_to_string (p1))) + " to ") + (point_to_string (p2))) + " is ") + (_str (d)))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_distance()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
