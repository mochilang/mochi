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
let rec mean (xs: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable xs = xs
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            sum <- sum + (_idx xs (i))
            i <- i + 1
        __ret <- sum / ((float (Seq.length (xs))) * 1.0)
        raise Return
        __ret
    with
        | Return -> __ret
and stump_predict (s: Stump) (x: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable s = s
    let mutable x = x
    try
        __ret <- if (_idx x (s._feature)) < (s._threshold) then (s._left) else (s._right)
        raise Return
        __ret
    with
        | Return -> __ret
and train_stump (features: float array array) (residuals: float array) =
    let mutable __ret : Stump = Unchecked.defaultof<Stump>
    let mutable features = features
    let mutable residuals = residuals
    try
        let mutable best_feature: int = 0
        let mutable best_threshold: float = 0.0
        let mutable best_error: float = 1000000000.0
        let mutable best_left: float = 0.0
        let mutable best_right: float = 0.0
        let num_features: int = Seq.length (_idx features (0))
        let mutable f: int = 0
        while f < num_features do
            let mutable i: int = 0
            while i < (Seq.length (features)) do
                let _threshold: float = _idx (_idx features (i)) (f)
                let mutable _left: float array = Array.empty<float>
                let mutable _right: float array = Array.empty<float>
                let mutable j: int = 0
                while j < (Seq.length (features)) do
                    if (_idx (_idx features (j)) (f)) < _threshold then
                        _left <- Array.append (_left) ([|_idx residuals (j)|])
                    else
                        _right <- Array.append (_right) ([|_idx residuals (j)|])
                    j <- j + 1
                if ((Seq.length (_left)) <> 0) && ((Seq.length (_right)) <> 0) then
                    let left_mean: float = mean (_left)
                    let right_mean: float = mean (_right)
                    let mutable err: float = 0.0
                    j <- 0
                    while j < (Seq.length (features)) do
                        let pred: float = if (_idx (_idx features (j)) (f)) < _threshold then left_mean else right_mean
                        let diff: float = (_idx residuals (j)) - pred
                        err <- err + (diff * diff)
                        j <- j + 1
                    if err < best_error then
                        best_error <- err
                        best_feature <- f
                        best_threshold <- _threshold
                        best_left <- left_mean
                        best_right <- right_mean
                i <- i + 1
            f <- f + 1
        __ret <- { _feature = best_feature; _threshold = best_threshold; _left = best_left; _right = best_right }
        raise Return
        __ret
    with
        | Return -> __ret
and boost (features: float array array) (targets: int array) (rounds: int) =
    let mutable __ret : Stump array = Unchecked.defaultof<Stump array>
    let mutable features = features
    let mutable targets = targets
    let mutable rounds = rounds
    try
        let mutable model: Stump array = Array.empty<Stump>
        let mutable preds: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (targets)) do
            preds <- Array.append (preds) ([|0.0|])
            i <- i + 1
        let mutable r: int = 0
        while r < rounds do
            let mutable residuals: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (targets)) do
                residuals <- Array.append (residuals) ([|(float (_idx targets (j))) - (_idx preds (j))|])
                j <- j + 1
            let stump: Stump = train_stump (features) (residuals)
            model <- Array.append (model) ([|stump|])
            j <- 0
            while j < (Seq.length (preds)) do
                preds.[j] <- (_idx preds (j)) + (stump_predict (stump) (_idx features (j)))
                j <- j + 1
            r <- r + 1
        __ret <- model
        raise Return
        __ret
    with
        | Return -> __ret
and predict (model: Stump array) (x: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable model = model
    let mutable x = x
    try
        let mutable score: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (model)) do
            let s: Stump = _idx model (i)
            if (_idx x (s._feature)) < (s._threshold) then
                score <- score + (s._left)
            else
                score <- score + (s._right)
            i <- i + 1
        __ret <- score
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let features: float array array = [|[|5.1; 3.5|]; [|4.9; 3.0|]; [|6.2; 3.4|]; [|5.9; 3.0|]|]
        let targets: int array = unbox<int array> [|0; 0; 1; 1|]
        let mutable model: Stump array = boost (features) (targets) (3)
        let mutable out: string = ""
        let mutable i: int = 0
        while i < (Seq.length (features)) do
            let s: float = predict (model) (_idx features (i))
            let label: int = if s >= 0.5 then 1 else 0
            if i = 0 then
                out <- _str (label)
            else
                out <- (out + " ") + (_str (label))
            i <- i + 1
        printfn "%s" (out)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
