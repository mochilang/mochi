// Generated 2025-08-08 17:07 +0700

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
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < n do
            result <- result * 10.0
            i <- i + 1
        __ret <- result
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
        let y: float = float (floor ((x * m) + 0.5))
        __ret <- y / m
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec mean (data: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable data = data
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        let n: int = Seq.length (data)
        while i < n do
            total <- total + (_idx data (i))
            i <- i + 1
        __ret <- total / (float n)
        raise Return
        __ret
    with
        | Return -> __ret
let rec stdev (data: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable data = data
    try
        let n: int = Seq.length (data)
        if n <= 1 then
            failwith ("data length must be > 1")
        let m: float = mean (data)
        let mutable sum_sq: float = 0.0
        let mutable i: int = 0
        while i < n do
            let diff: float = (_idx data (i)) - m
            sum_sq <- sum_sq + (diff * diff)
            i <- i + 1
        __ret <- sqrtApprox (sum_sq / (float (n - 1)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec normalization (data: float array) (ndigits: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable data = data
    let mutable ndigits = ndigits
    try
        let x_min: float = float (Seq.min (data))
        let x_max: float = float (Seq.max (data))
        let denom: float = x_max - x_min
        let mutable result: float array = Array.empty<float>
        let mutable i: int = 0
        let n: int = Seq.length (data)
        while i < n do
            let norm: float = ((_idx data (i)) - x_min) / denom
            result <- Array.append result [|(round (norm) (ndigits))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec standardization (data: float array) (ndigits: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable data = data
    let mutable ndigits = ndigits
    try
        let mu: float = mean (data)
        let sigma: float = stdev (data)
        let mutable result: float array = Array.empty<float>
        let mutable i: int = 0
        let n: int = Seq.length (data)
        while i < n do
            let z: float = ((_idx data (i)) - mu) / sigma
            result <- Array.append result [|(round (z) (ndigits))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (normalization (unbox<float array> [|2.0; 7.0; 10.0; 20.0; 30.0; 50.0|]) (3)))
printfn "%s" (_str (normalization (unbox<float array> [|5.0; 10.0; 15.0; 20.0; 25.0|]) (3)))
printfn "%s" (_str (standardization (unbox<float array> [|2.0; 7.0; 10.0; 20.0; 30.0; 50.0|]) (3)))
printfn "%s" (_str (standardization (unbox<float array> [|5.0; 10.0; 15.0; 20.0; 25.0|]) (3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
