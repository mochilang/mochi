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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let N_STEPS: int = 1000
let rec floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable p: float = 1.0
        let mutable i: int = 0
        while i < n do
            p <- p * 10.0
            i <- i + 1
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
let rec round (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let m: float = pow10 (n)
        __ret <- (floor ((x * m) + 0.5)) / m
        raise Return
        __ret
    with
        | Return -> __ret
let rec simpson_integration (f: float -> float) (a: float) (b: float) (precision: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable a = a
    let mutable b = b
    let mutable precision = precision
    try
        if precision <= 0 then
            failwith ("precision should be positive")
        let h: float = (b - a) / (float N_STEPS)
        let mutable result: float = (float (f (a))) + (float (f (b)))
        let mutable i: int = 1
        while i < N_STEPS do
            let x: float = a + (h * (float i))
            if (((i % 2 + 2) % 2)) = 1 then
                result <- result + (float (4.0 * (float (f (x)))))
            else
                result <- result + (float (2.0 * (float (f (x)))))
            i <- i + 1
        result <- result * (h / 3.0)
        let r: float = round (result) (precision)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec square (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- x * x
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (simpson_integration (unbox<float -> float> square) (1.0) (2.0) (3)))
printfn "%s" (_str (simpson_integration (unbox<float -> float> square) (3.45) (3.2) (1)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
