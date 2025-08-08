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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
type Dataset = {
    mutable _data: float array array
    mutable _target: float array
}
type Tree = {
    mutable _threshold: float
    mutable _left_value: float
    mutable _right_value: float
}
let rec data_handling (dataset: Dataset) =
    let mutable __ret : Dataset = Unchecked.defaultof<Dataset>
    let mutable dataset = dataset
    try
        __ret <- dataset
        raise Return
        __ret
    with
        | Return -> __ret
and xgboost (features: float array array) (_target: float array) (test_features: float array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable features = features
    let mutable _target = _target
    let mutable test_features = test_features
    try
        let learning_rate: float = 0.5
        let n_estimators: int = 3
        let mutable trees: Tree array = Array.empty<Tree>
        let mutable predictions: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (_target)) do
            predictions <- Array.append predictions [|0.0|]
            i <- i + 1
        let mutable est: int = 0
        while est < n_estimators do
            let mutable residuals: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (_target)) do
                residuals <- Array.append residuals [|((_idx _target (j)) - (_idx predictions (j)))|]
                j <- j + 1
            let mutable sum_feat: float = 0.0
            j <- 0
            while j < (Seq.length (features)) do
                sum_feat <- sum_feat + (_idx (_idx features (j)) (0))
                j <- j + 1
            let _threshold: float = sum_feat / (float (Seq.length (features)))
            let mutable left_sum: float = 0.0
            let mutable left_count: int = 0
            let mutable right_sum: float = 0.0
            let mutable right_count: int = 0
            j <- 0
            while j < (Seq.length (features)) do
                if (_idx (_idx features (j)) (0)) <= _threshold then
                    left_sum <- left_sum + (_idx residuals (j))
                    left_count <- left_count + 1
                else
                    right_sum <- right_sum + (_idx residuals (j))
                    right_count <- right_count + 1
                j <- j + 1
            let mutable _left_value: float = 0.0
            if left_count > 0 then
                _left_value <- left_sum / (float left_count)
            let mutable _right_value: float = 0.0
            if right_count > 0 then
                _right_value <- right_sum / (float right_count)
            j <- 0
            while j < (Seq.length (features)) do
                if (_idx (_idx features (j)) (0)) <= _threshold then
                    predictions.[j] <- (_idx predictions (j)) + (learning_rate * _left_value)
                else
                    predictions.[j] <- (_idx predictions (j)) + (learning_rate * _right_value)
                j <- j + 1
            trees <- Array.append trees [|{ _threshold = _threshold; _left_value = _left_value; _right_value = _right_value }|]
            est <- est + 1
        let mutable preds: float array = Array.empty<float>
        let mutable t: int = 0
        while t < (Seq.length (test_features)) do
            let mutable pred: float = 0.0
            let mutable k: int = 0
            while k < (Seq.length (trees)) do
                if (_idx (_idx test_features (t)) (0)) <= ((_idx trees (k))._threshold) then
                    pred <- pred + (learning_rate * ((_idx trees (k))._left_value))
                else
                    pred <- pred + (learning_rate * ((_idx trees (k))._right_value))
                k <- k + 1
            preds <- Array.append preds [|pred|]
            t <- t + 1
        __ret <- preds
        raise Return
        __ret
    with
        | Return -> __ret
and mean_absolute_error (y_true: float array) (y_pred: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let mutable diff: float = (_idx y_true (i)) - (_idx y_pred (i))
            if diff < 0.0 then
                diff <- -diff
            sum <- sum + diff
            i <- i + 1
        __ret <- sum / (float (Seq.length (y_true)))
        raise Return
        __ret
    with
        | Return -> __ret
and mean_squared_error (y_true: float array) (y_pred: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let mutable diff: float = (_idx y_true (i)) - (_idx y_pred (i))
            sum <- sum + (diff * diff)
            i <- i + 1
        __ret <- sum / (float (Seq.length (y_true)))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let california: Dataset = { _data = [|[|1.0|]; [|2.0|]; [|3.0|]; [|4.0|]|]; _target = unbox<float array> [|2.0; 3.0; 4.0; 5.0|] }
        let ds: Dataset = data_handling (california)
        let x_train: float array array = ds._data
        let y_train: float array = ds._target
        let x_test: float array array = [|[|1.5|]; [|3.5|]|]
        let y_test: float array = unbox<float array> [|2.5; 4.5|]
        let mutable predictions: float array = xgboost (x_train) (y_train) (x_test)
        printfn "%s" ("Predictions:")
        printfn "%s" (_repr (predictions))
        printfn "%s" ("Mean Absolute Error:")
        printfn "%g" (mean_absolute_error (y_test) (predictions))
        printfn "%s" ("Mean Square Error:")
        printfn "%g" (mean_squared_error (y_test) (predictions))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
