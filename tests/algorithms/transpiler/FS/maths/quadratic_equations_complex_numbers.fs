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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Complex = {
    mutable _re: float
    mutable _im: float
}
let rec add (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _re = (a._re) + (b._re); _im = (a._im) + (b._im) }
        raise Return
        __ret
    with
        | Return -> __ret
and sub (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _re = (a._re) - (b._re); _im = (a._im) - (b._im) }
        raise Return
        __ret
    with
        | Return -> __ret
and div_real (a: Complex) (r: float) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable r = r
    try
        __ret <- { _re = (a._re) / r; _im = (a._im) / r }
        raise Return
        __ret
    with
        | Return -> __ret
and sqrt_newton (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
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
and sqrt_to_complex (d: float) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable d = d
    try
        __ret <- if d >= 0.0 then { _re = sqrt_newton (d); _im = 0.0 } else { _re = 0.0; _im = sqrt_newton (-d) }
        raise Return
        __ret
    with
        | Return -> __ret
and quadratic_roots (a: float) (b: float) (c: float) =
    let mutable __ret : Complex array = Unchecked.defaultof<Complex array>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        if a = 0.0 then
            printfn "%s" ("ValueError: coefficient 'a' must not be zero")
            __ret <- Array.empty<Complex>
            raise Return
        let delta: float = (b * b) - ((4.0 * a) * c)
        let sqrt_d: Complex = sqrt_to_complex (delta)
        let minus_b: Complex = { _re = -b; _im = 0.0 }
        let two_a: float = 2.0 * a
        let root1: Complex = div_real (add (minus_b) (sqrt_d)) (two_a)
        let root2: Complex = div_real (sub (minus_b) (sqrt_d)) (two_a)
        __ret <- unbox<Complex array> [|root1; root2|]
        raise Return
        __ret
    with
        | Return -> __ret
and root_str (r: Complex) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable r = r
    try
        if (r._im) = 0.0 then
            __ret <- _str (r._re)
            raise Return
        let mutable s: string = _str (r._re)
        if (r._im) >= 0.0 then
            s <- ((s + "+") + (_str (r._im))) + "i"
        else
            s <- (s + (_str (r._im))) + "i"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let roots: Complex array = quadratic_roots (5.0) (6.0) (1.0)
        if (Seq.length (roots)) = 2 then
            printfn "%s" ((("The solutions are: " + (root_str (_idx roots (0)))) + " and ") + (root_str (_idx roots (1))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
