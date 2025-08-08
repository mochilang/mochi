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
type Stump = {
    mutable _feature: int
    mutable _threshold: float
    mutable _left: float
    mutable _right: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec exp_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            term <- (term * x) / (float i)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec signf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x >= 0.0 then 1.0 else (-1.0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec gradient (target: float array) (preds: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable target = target
    let mutable preds = preds
    try
        let n: int = Seq.length (target)
        let mutable residuals: float array = Array.empty<float>
        let mutable i: int = 0
        while i < n do
            let t: float = _idx target (i)
            let y: float = _idx preds (i)
            let exp_val: float = exp_approx (t * y)
            let res: float = (-t) / (1.0 + exp_val)
            residuals <- Array.append residuals [|res|]
            i <- i + 1
        __ret <- residuals
        raise Return
        __ret
    with
        | Return -> __ret
let rec predict_raw (models: Stump array) (features: float array array) (learning_rate: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable models = models
    let mutable features = features
    let mutable learning_rate = learning_rate
    try
        let n: int = Seq.length (features)
        let mutable preds: float array = Array.empty<float>
        let mutable i: int = 0
        while i < n do
            preds <- Array.append preds [|0.0|]
            i <- i + 1
        let mutable m: int = 0
        while m < (Seq.length (models)) do
            let stump: Stump = _idx models (m)
            i <- 0
            while i < n do
                let value: float = _idx (_idx features (i)) (stump._feature)
                if value <= (stump._threshold) then
                    preds.[i] <- (_idx preds (i)) + (learning_rate * (stump._left))
                else
                    preds.[i] <- (_idx preds (i)) + (learning_rate * (stump._right))
                i <- i + 1
            m <- m + 1
        __ret <- preds
        raise Return
        __ret
    with
        | Return -> __ret
let rec predict (models: Stump array) (features: float array array) (learning_rate: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable models = models
    let mutable features = features
    let mutable learning_rate = learning_rate
    try
        let raw: float array = predict_raw (models) (features) (learning_rate)
        let mutable result: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (raw)) do
            result <- Array.append result [|(signf (_idx raw (i)))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec train_stump (features: float array array) (residuals: float array) =
    let mutable __ret : Stump = Unchecked.defaultof<Stump>
    let mutable features = features
    let mutable residuals = residuals
    try
        let n_samples: int = Seq.length (features)
        let n_features: int = Seq.length (_idx features (0))
        let mutable best_feature: int = 0
        let mutable best_threshold: float = 0.0
        let mutable best_error: float = 1000000000.0
        let mutable best_left: float = 0.0
        let mutable best_right: float = 0.0
        let mutable j: int = 0
        while j < n_features do
            let mutable t_index: int = 0
            while t_index < n_samples do
                let t: float = _idx (_idx features (t_index)) (j)
                let mutable sum_left: float = 0.0
                let mutable count_left: int = 0
                let mutable sum_right: float = 0.0
                let mutable count_right: int = 0
                let mutable i: int = 0
                while i < n_samples do
                    if (_idx (_idx features (i)) (j)) <= t then
                        sum_left <- sum_left + (_idx residuals (i))
                        count_left <- count_left + 1
                    else
                        sum_right <- sum_right + (_idx residuals (i))
                        count_right <- count_right + 1
                    i <- i + 1
                let mutable left_val: float = 0.0
                if count_left <> 0 then
                    left_val <- sum_left / (float count_left)
                let mutable right_val: float = 0.0
                if count_right <> 0 then
                    right_val <- sum_right / (float count_right)
                let mutable error: float = 0.0
                i <- 0
                while i < n_samples do
                    let pred: float = if (_idx (_idx features (i)) (j)) <= t then left_val else right_val
                    let diff: float = (_idx residuals (i)) - pred
                    error <- error + (diff * diff)
                    i <- i + 1
                if error < best_error then
                    best_error <- error
                    best_feature <- j
                    best_threshold <- t
                    best_left <- left_val
                    best_right <- right_val
                t_index <- t_index + 1
            j <- j + 1
        __ret <- { _feature = best_feature; _threshold = best_threshold; _left = best_left; _right = best_right }
        raise Return
        __ret
    with
        | Return -> __ret
let rec fit (n_estimators: int) (learning_rate: float) (features: float array array) (target: float array) =
    let mutable __ret : Stump array = Unchecked.defaultof<Stump array>
    let mutable n_estimators = n_estimators
    let mutable learning_rate = learning_rate
    let mutable features = features
    let mutable target = target
    try
        let mutable models: Stump array = Array.empty<Stump>
        let mutable m: int = 0
        while m < n_estimators do
            let mutable preds: float array = predict_raw (models) (features) (learning_rate)
            let grad: float array = gradient (target) (preds)
            let mutable residuals: float array = Array.empty<float>
            let mutable i: int = 0
            while i < (Seq.length (grad)) do
                residuals <- Array.append residuals [|(-(_idx grad (i)))|]
                i <- i + 1
            let stump: Stump = train_stump (features) (residuals)
            models <- Array.append models [|stump|]
            m <- m + 1
        __ret <- models
        raise Return
        __ret
    with
        | Return -> __ret
let rec accuracy (preds: float array) (target: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable preds = preds
    let mutable target = target
    try
        let n: int = Seq.length (target)
        let mutable correct: int = 0
        let mutable i: int = 0
        while i < n do
            if (_idx preds (i)) = (_idx target (i)) then
                correct <- correct + 1
            i <- i + 1
        __ret <- (float correct) / (float n)
        raise Return
        __ret
    with
        | Return -> __ret
let features: float array array = [|[|1.0|]; [|2.0|]; [|3.0|]; [|4.0|]|]
let target: float array = unbox<float array> [|-1.0; -1.0; 1.0; 1.0|]
let mutable models: Stump array = fit (5) (0.5) (features) (target)
let predictions: float array = predict (models) (features) (0.5)
let acc: float = accuracy (predictions) (target)
printfn "%s" ("Accuracy: " + (_str (acc)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
