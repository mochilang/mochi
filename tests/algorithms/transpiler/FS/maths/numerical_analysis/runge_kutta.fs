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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec runge_kutta (f: float -> float -> float) (y0: float) (x0: float) (h: float) (x_end: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable f = f
    let mutable y0 = y0
    let mutable x0 = x0
    let mutable h = h
    let mutable x_end = x_end
    try
        let span: float = (x_end - x0) / h
        let mutable n: int = int (span)
        if (float (float (n))) < span then
            n <- n + 1
        let mutable y: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (n + 1) do
            y <- Array.append y [|0.0|]
            i <- i + 1
        y.[0] <- y0
        let mutable x: float = x0
        let mutable k: int = 0
        while k < n do
            let k1: float = f (x) (_idx y (k))
            let k2: float = f (x + (0.5 * h)) ((_idx y (k)) + ((0.5 * h) * k1))
            let k3: float = f (x + (0.5 * h)) ((_idx y (k)) + ((0.5 * h) * k2))
            let k4: float = f (x + h) ((_idx y (k)) + (h * k3))
            y.[k + 1] <- (_idx y (k)) + (((1.0 / 6.0) * h) * (((k1 + (2.0 * k2)) + (2.0 * k3)) + k4))
            x <- x + h
            k <- k + 1
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
and test_runge_kutta () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let rec f (x: float) (y: float) =
            let mutable __ret : float = Unchecked.defaultof<float>
            let mutable x = x
            let mutable y = y
            try
                __ret <- y
                raise Return
                __ret
            with
                | Return -> __ret
        let result: float array = runge_kutta (unbox<float -> float -> float> f) (1.0) (0.0) (0.01) (5.0)
        let last: float = _idx result ((Seq.length (result)) - 1)
        let expected: float = 148.41315904125113
        let mutable diff: float = last - expected
        if diff < 0.0 then
            diff <- -diff
        if diff > 0.000001 then
            failwith ("runge_kutta failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_runge_kutta()
        let rec f (x: float) (y: float) =
            let mutable __ret : float = Unchecked.defaultof<float>
            let mutable x = x
            let mutable y = y
            try
                __ret <- y
                raise Return
                __ret
            with
                | Return -> __ret
        let r: float array = runge_kutta (unbox<float -> float -> float> f) (1.0) (0.0) (0.1) (1.0)
        printfn "%s" (_str (_idx r ((Seq.length (r)) - 1)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
