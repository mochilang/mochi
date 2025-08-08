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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec f (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (x - 0.0) * (x - 0.0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_points (a: float) (b: float) (h: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    let mutable h = h
    try
        let mutable points: float array = Array.empty<float>
        let mutable x: float = a + h
        while x < (b - h) do
            points <- Array.append points [|x|]
            x <- x + h
        __ret <- points
        raise Return
        __ret
    with
        | Return -> __ret
let rec simpson_rule (boundary: float array) (steps: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable boundary = boundary
    let mutable steps = steps
    try
        if steps <= 0 then
            failwith ("Number of steps must be greater than zero")
        let a: float = _idx boundary (0)
        let b: float = _idx boundary (1)
        let h: float = (b - a) / (float steps)
        let pts: float array = make_points (a) (b) (h)
        let mutable y: float = (h / 3.0) * (f (a))
        let mutable cnt: int = 2
        let mutable i: int = 0
        while i < (Seq.length (pts)) do
            let coeff: float = 4.0 - (2.0 * (float (((cnt % 2 + 2) % 2))))
            y <- y + (((h / 3.0) * coeff) * (f (_idx pts (i))))
            cnt <- cnt + 1
            i <- i + 1
        y <- y + ((h / 3.0) * (f (b)))
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
let result: float = simpson_rule (unbox<float array> [|0.0; 1.0|]) (10)
printfn "%s" (_str (result))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
