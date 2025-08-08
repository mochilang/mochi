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
let rec ucal (u: float) (p: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable u = u
    let mutable p = p
    try
        let mutable temp: float = u
        let mutable i: int = 1
        while i < p do
            temp <- temp * (u - (float i))
            i <- i + 1
        __ret <- temp
        raise Return
        __ret
    with
        | Return -> __ret
let rec factorial (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable result: float = 1.0
        let mutable i: int = 2
        while i <= n do
            result <- result * (float i)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec newton_forward_interpolation (x: float array) (y0: float array) (value: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y0 = y0
    let mutable value = value
    try
        let n: int = Seq.length (x)
        let mutable y: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|0.0|]
                j <- j + 1
            y <- Array.append y [|row|]
            i <- i + 1
        i <- 0
        while i < n do
            y.[i].[0] <- _idx y0 (i)
            i <- i + 1
        let mutable i1: int = 1
        while i1 < n do
            let mutable j1: int = 0
            while j1 < (n - i1) do
                y.[j1].[i1] <- (_idx (_idx y (j1 + 1)) (i1 - 1)) - (_idx (_idx y (j1)) (i1 - 1))
                j1 <- j1 + 1
            i1 <- i1 + 1
        let u: float = (value - (_idx x (0))) / ((_idx x (1)) - (_idx x (0)))
        let mutable sum: float = _idx (_idx y (0)) (0)
        let mutable k: int = 1
        while k < n do
            sum <- sum + (((ucal (u) (k)) * (_idx (_idx y (0)) (k))) / (factorial (k)))
            k <- k + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let x_points: float array = unbox<float array> [|0.0; 1.0; 2.0; 3.0|]
let y_points: float array = unbox<float array> [|0.0; 1.0; 8.0; 27.0|]
printfn "%s" (_str (newton_forward_interpolation (x_points) (y_points) (1.5)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
