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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec exp_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: float = 1.0
        while i < 20.0 do
            term <- (term * x) / i
            sum <- sum + term
            i <- i + 1.0
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec sigmoid (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- 1.0 / (1.0 + (exp_taylor (-x)))
        raise Return
        __ret
    with
        | Return -> __ret
let X: float array array = [|[|0.0; 0.0|]; [|1.0; 1.0|]; [|1.0; 0.0|]; [|0.0; 1.0|]|]
let Y: float array = unbox<float array> [|0.0; 1.0; 0.0; 0.0|]
let test_data: float array array = [|[|0.0; 0.0|]; [|0.0; 1.0|]; [|1.0; 1.0|]|]
let mutable w1: float array array = [|[|0.5; -0.5|]; [|0.5; 0.5|]|]
let mutable b1: float array = unbox<float array> [|0.0; 0.0|]
let mutable w2: float array = unbox<float array> [|0.5; -0.5|]
let mutable b2: float = 0.0
let rec train (epochs: int) (lr: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable epochs = epochs
    let mutable lr = lr
    try
        let mutable e: int = 0
        while e < epochs do
            let mutable i: int = 0
            while i < (Seq.length (X)) do
                let x0: float = _idx (_idx X (i)) (0)
                let x1: float = _idx (_idx X (i)) (1)
                let target: float = _idx Y (i)
                let z1: float = (((_idx (_idx w1 (0)) (0)) * x0) + ((_idx (_idx w1 (1)) (0)) * x1)) + (_idx b1 (0))
                let z2: float = (((_idx (_idx w1 (0)) (1)) * x0) + ((_idx (_idx w1 (1)) (1)) * x1)) + (_idx b1 (1))
                let h1: float = sigmoid (z1)
                let h2: float = sigmoid (z2)
                let z3: float = (((_idx w2 (0)) * h1) + ((_idx w2 (1)) * h2)) + b2
                let out: float = sigmoid (z3)
                let error: float = out - target
                let d1: float = ((h1 * (1.0 - h1)) * (_idx w2 (0))) * error
                let d2: float = ((h2 * (1.0 - h2)) * (_idx w2 (1))) * error
                w2 <- _arrset w2 (0) ((_idx w2 (0)) - ((lr * error) * h1))
                w2 <- _arrset w2 (1) ((_idx w2 (1)) - ((lr * error) * h2))
                b2 <- b2 - (lr * error)
                w1.[0].[0] <- (_idx (_idx w1 (0)) (0)) - ((lr * d1) * x0)
                w1.[1].[0] <- (_idx (_idx w1 (1)) (0)) - ((lr * d1) * x1)
                b1 <- _arrset b1 (0) ((_idx b1 (0)) - (lr * d1))
                w1.[0].[1] <- (_idx (_idx w1 (0)) (1)) - ((lr * d2) * x0)
                w1.[1].[1] <- (_idx (_idx w1 (1)) (1)) - ((lr * d2) * x1)
                b1 <- _arrset b1 (1) ((_idx b1 (1)) - (lr * d2))
                i <- i + 1
            e <- e + 1
        __ret
    with
        | Return -> __ret
let rec predict (samples: float array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable samples = samples
    try
        let mutable preds: int array = Array.empty<int>
        let mutable i: int = 0
        while i < (Seq.length (samples)) do
            let x0: float = _idx (_idx samples (i)) (0)
            let x1: float = _idx (_idx samples (i)) (1)
            let z1: float = (((_idx (_idx w1 (0)) (0)) * x0) + ((_idx (_idx w1 (1)) (0)) * x1)) + (_idx b1 (0))
            let z2: float = (((_idx (_idx w1 (0)) (1)) * x0) + ((_idx (_idx w1 (1)) (1)) * x1)) + (_idx b1 (1))
            let h1: float = sigmoid (z1)
            let h2: float = sigmoid (z2)
            let z3: float = (((_idx w2 (0)) * h1) + ((_idx w2 (1)) * h2)) + b2
            let out: float = sigmoid (z3)
            let mutable label: int = 0
            if out >= 0.5 then
                label <- 1
            preds <- Array.append preds [|label|]
            i <- i + 1
        __ret <- preds
        raise Return
        __ret
    with
        | Return -> __ret
let rec wrapper (y: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable y = y
    try
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
train (4000) (0.5)
let mutable preds: int array = wrapper (predict (test_data))
printfn "%s" (_str (preds))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
