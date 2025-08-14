// Generated 2025-08-14 17:48 +0700

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
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
type Dual = {
    mutable _value: float
    mutable _deriv: float
}
let rec dual (v: float) (d: float) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable v = v
    let mutable d = d
    try
        __ret <- { _value = v; _deriv = d }
        raise Return
        __ret
    with
        | Return -> __ret
and pow_float (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable res: float = 1.0
        let mutable i: int = 0
        while i < exp do
            res <- res * ``base``
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and add (a: Dual) (b: Dual) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _value = (a._value) + (b._value); _deriv = (a._deriv) + (b._deriv) }
        raise Return
        __ret
    with
        | Return -> __ret
and sub (a: Dual) (b: Dual) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _value = (a._value) - (b._value); _deriv = (a._deriv) - (b._deriv) }
        raise Return
        __ret
    with
        | Return -> __ret
and mul (a: Dual) (b: Dual) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _value = (a._value) * (b._value); _deriv = ((a._deriv) * (b._value)) + ((b._deriv) * (a._value)) }
        raise Return
        __ret
    with
        | Return -> __ret
and div (a: Dual) (b: Dual) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { _value = (a._value) / (b._value); _deriv = (((a._deriv) * (b._value)) - ((b._deriv) * (a._value))) / ((b._value) * (b._value)) }
        raise Return
        __ret
    with
        | Return -> __ret
and power (a: Dual) (p: int) =
    let mutable __ret : Dual = Unchecked.defaultof<Dual>
    let mutable a = a
    let mutable p = p
    try
        __ret <- { _value = pow_float (a._value) (p); _deriv = ((1.0 * (float p)) * (pow_float (a._value) (p - 1))) * (a._deriv) }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let a: Dual = dual (2.0) (1.0)
        let b: Dual = dual (1.0) (0.0)
        let c: Dual = add (a) (b)
        let d: Dual = mul (a) (b)
        let e: Dual = div (c) (d)
        ignore (printfn "%s" (_str (e._deriv)))
        let x: Dual = dual (2.0) (1.0)
        let y: Dual = power (x) (3)
        ignore (printfn "%s" (_str (y._deriv)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
