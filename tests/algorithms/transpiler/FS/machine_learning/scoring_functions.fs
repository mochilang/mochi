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
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (0.0 - x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
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
and ln_series (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let t: float = (x - 1.0) / (x + 1.0)
        let mutable term: float = t
        let mutable sum: float = 0.0
        let mutable n: int = 1
        while n <= 19 do
            sum <- sum + (term / (float n))
            term <- (term * t) * t
            n <- n + 2
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
and ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = x
        let mutable k: int = 0
        while y >= 10.0 do
            y <- y / 10.0
            k <- k + 1
        while y < 1.0 do
            y <- y * 10.0
            k <- k - 1
        __ret <- (ln_series (y)) + ((float k) * (ln_series (10.0)))
        raise Return
        __ret
    with
        | Return -> __ret
and mae (predict: float array) (actual: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable predict = predict
    let mutable actual = actual
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (predict)) do
            let diff: float = (_idx predict (i)) - (_idx actual (i))
            sum <- sum + (absf (diff))
            i <- i + 1
        __ret <- sum / (float (Seq.length (predict)))
        raise Return
        __ret
    with
        | Return -> __ret
and mse (predict: float array) (actual: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable predict = predict
    let mutable actual = actual
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (predict)) do
            let diff: float = (_idx predict (i)) - (_idx actual (i))
            sum <- sum + (diff * diff)
            i <- i + 1
        __ret <- sum / (float (Seq.length (predict)))
        raise Return
        __ret
    with
        | Return -> __ret
and rmse (predict: float array) (actual: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable predict = predict
    let mutable actual = actual
    try
        __ret <- sqrtApprox (mse (predict) (actual))
        raise Return
        __ret
    with
        | Return -> __ret
and rmsle (predict: float array) (actual: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable predict = predict
    let mutable actual = actual
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (predict)) do
            let lp: float = ln ((_idx predict (i)) + 1.0)
            let la: float = ln ((_idx actual (i)) + 1.0)
            let diff: float = lp - la
            sum <- sum + (diff * diff)
            i <- i + 1
        __ret <- sqrtApprox (sum / (float (Seq.length (predict))))
        raise Return
        __ret
    with
        | Return -> __ret
and mbd (predict: float array) (actual: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable predict = predict
    let mutable actual = actual
    try
        let mutable diff_sum: float = 0.0
        let mutable actual_sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (predict)) do
            diff_sum <- diff_sum + ((_idx predict (i)) - (_idx actual (i)))
            actual_sum <- actual_sum + (_idx actual (i))
            i <- i + 1
        let mutable n: float = float (Seq.length (predict))
        let numerator: float = diff_sum / n
        let denominator: float = actual_sum / n
        __ret <- (numerator / denominator) * 100.0
        raise Return
        __ret
    with
        | Return -> __ret
and manual_accuracy (predict: float array) (actual: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable predict = predict
    let mutable actual = actual
    try
        let mutable correct: int = 0
        let mutable i: int = 0
        while i < (Seq.length (predict)) do
            if (_idx predict (i)) = (_idx actual (i)) then
                correct <- correct + 1
            i <- i + 1
        __ret <- (float correct) / (float (Seq.length (predict)))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let actual: float array = unbox<float array> [|1.0; 2.0; 3.0|]
        let predict: float array = unbox<float array> [|1.0; 4.0; 3.0|]
        printfn "%s" (_str (mae (predict) (actual)))
        printfn "%s" (_str (mse (predict) (actual)))
        printfn "%s" (_str (rmse (predict) (actual)))
        printfn "%s" (_str (rmsle (unbox<float array> [|10.0; 2.0; 30.0|]) (unbox<float array> [|10.0; 10.0; 30.0|])))
        printfn "%s" (_str (mbd (unbox<float array> [|2.0; 3.0; 4.0|]) (unbox<float array> [|1.0; 2.0; 3.0|])))
        printfn "%s" (_str (mbd (unbox<float array> [|0.0; 1.0; 1.0|]) (unbox<float array> [|1.0; 2.0; 3.0|])))
        printfn "%s" (_str (manual_accuracy (predict) (actual)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
