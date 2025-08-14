// Generated 2025-08-14 17:48 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%g" f
    | _ ->
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
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and maxf (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and minf (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and clip (x: float) (lo: float) (hi: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable lo = lo
    let mutable hi = hi
    try
        __ret <- maxf (lo) (minf (x) (hi))
        raise Return
        __ret
    with
        | Return -> __ret
and to_float (x: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (float x) * 1.0
        raise Return
        __ret
    with
        | Return -> __ret
and powf (``base``: float) (exp: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        let mutable n: int = int (exp)
        while i < n do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            ignore (failwith ("ln domain error"))
        let y: float = (x - 1.0) / (x + 1.0)
        let y2: float = y * y
        let mutable term: float = y
        let mutable sum: float = 0.0
        let mutable k: int = 0
        while k < 10 do
            let denom: float = to_float ((2 * k) + 1)
            sum <- sum + (term / denom)
            term <- term * y2
            k <- k + 1
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
and exp (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- System.Math.Exp(x)
        raise Return
        __ret
    with
        | Return -> __ret
and mean (v: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable v = v
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (v)) do
            total <- total + (_idx v (int i))
            i <- i + 1
        __ret <- total / (to_float (Seq.length (v)))
        raise Return
        __ret
    with
        | Return -> __ret
and binary_cross_entropy (y_true: float array) (y_pred: float array) (epsilon: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    let mutable epsilon = epsilon
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Input arrays must have the same length."))
        let mutable losses: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let mutable yt: float = _idx y_true (int i)
            let yp: float = clip (_idx y_pred (int i)) (epsilon) (1.0 - epsilon)
            let loss: float = -((yt * (ln (yp))) + ((1.0 - yt) * (ln (1.0 - yp))))
            losses <- Array.append losses [|loss|]
            i <- i + 1
        __ret <- mean (losses)
        raise Return
        __ret
    with
        | Return -> __ret
and binary_focal_cross_entropy (y_true: float array) (y_pred: float array) (gamma: float) (alpha: float) (epsilon: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    let mutable gamma = gamma
    let mutable alpha = alpha
    let mutable epsilon = epsilon
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Input arrays must have the same length."))
        let mutable losses: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let mutable yt: float = _idx y_true (int i)
            let yp: float = clip (_idx y_pred (int i)) (epsilon) (1.0 - epsilon)
            let term1: float = ((alpha * (powf (1.0 - yp) (gamma))) * yt) * (ln (yp))
            let term2: float = (((1.0 - alpha) * (powf (yp) (gamma))) * (1.0 - yt)) * (ln (1.0 - yp))
            losses <- Array.append losses [|(-(term1 + term2))|]
            i <- i + 1
        __ret <- mean (losses)
        raise Return
        __ret
    with
        | Return -> __ret
and categorical_cross_entropy (y_true: float array array) (y_pred: float array array) (epsilon: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    let mutable epsilon = epsilon
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Input arrays must have the same shape."))
        let rows: int = Seq.length (y_true)
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < rows do
            if (Seq.length (_idx y_true (int i))) <> (Seq.length (_idx y_pred (int i))) then
                ignore (failwith ("Input arrays must have the same shape."))
            let mutable sum_true: float = 0.0
            let mutable sum_pred: float = 0.0
            let mutable j: int = 0
            while j < (Seq.length (_idx y_true (int i))) do
                let mutable yt: float = _idx (_idx y_true (int i)) (int j)
                let yp: float = _idx (_idx y_pred (int i)) (int j)
                if (yt <> 0.0) && (yt <> 1.0) then
                    ignore (failwith ("y_true must be one-hot encoded."))
                sum_true <- sum_true + yt
                sum_pred <- sum_pred + yp
                j <- j + 1
            if sum_true <> 1.0 then
                ignore (failwith ("y_true must be one-hot encoded."))
            if (absf (sum_pred - 1.0)) > epsilon then
                ignore (failwith ("Predicted probabilities must sum to approximately 1."))
            j <- 0
            while j < (Seq.length (_idx y_true (int i))) do
                let yp: float = clip (_idx (_idx y_pred (int i)) (int j)) (epsilon) (1.0)
                total <- total - ((_idx (_idx y_true (int i)) (int j)) * (ln (yp)))
                j <- j + 1
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and categorical_focal_cross_entropy (y_true: float array array) (y_pred: float array array) (alpha: float array) (gamma: float) (epsilon: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    let mutable alpha = alpha
    let mutable gamma = gamma
    let mutable epsilon = epsilon
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Shape of y_true and y_pred must be the same."))
        let rows: int = Seq.length (y_true)
        let cols: int = Seq.length (_idx y_true (int 0))
        let mutable a: float array = alpha
        if (Seq.length (a)) = 0 then
            let mutable tmp: float array = Array.empty<float>
            let mutable j: int = 0
            while j < cols do
                tmp <- Array.append tmp [|1.0|]
                j <- j + 1
            a <- tmp
        if (Seq.length (a)) <> cols then
            ignore (failwith ("Length of alpha must match the number of classes."))
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < rows do
            if ((Seq.length (_idx y_true (int i))) <> cols) || ((Seq.length (_idx y_pred (int i))) <> cols) then
                ignore (failwith ("Shape of y_true and y_pred must be the same."))
            let mutable sum_true: float = 0.0
            let mutable sum_pred: float = 0.0
            let mutable j: int = 0
            while j < cols do
                let mutable yt: float = _idx (_idx y_true (int i)) (int j)
                let yp: float = _idx (_idx y_pred (int i)) (int j)
                if (yt <> 0.0) && (yt <> 1.0) then
                    ignore (failwith ("y_true must be one-hot encoded."))
                sum_true <- sum_true + yt
                sum_pred <- sum_pred + yp
                j <- j + 1
            if sum_true <> 1.0 then
                ignore (failwith ("y_true must be one-hot encoded."))
            if (absf (sum_pred - 1.0)) > epsilon then
                ignore (failwith ("Predicted probabilities must sum to approximately 1."))
            let mutable row_loss: float = 0.0
            j <- 0
            while j < cols do
                let yp: float = clip (_idx (_idx y_pred (int i)) (int j)) (epsilon) (1.0)
                row_loss <- row_loss + ((((_idx a (int j)) * (powf (1.0 - yp) (gamma))) * (_idx (_idx y_true (int i)) (int j))) * (ln (yp)))
                j <- j + 1
            total <- total - row_loss
            i <- i + 1
        __ret <- total / (to_float (rows))
        raise Return
        __ret
    with
        | Return -> __ret
and hinge_loss (y_true: float array) (y_pred: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Length of predicted and actual array must be same."))
        let mutable losses: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let mutable yt: float = _idx y_true (int i)
            if (yt <> (-1.0)) && (yt <> 1.0) then
                ignore (failwith ("y_true can have values -1 or 1 only."))
            let pred: float = _idx y_pred (int i)
            let l: float = maxf (0.0) (1.0 - (yt * pred))
            losses <- Array.append losses [|l|]
            i <- i + 1
        __ret <- mean (losses)
        raise Return
        __ret
    with
        | Return -> __ret
and huber_loss (y_true: float array) (y_pred: float array) (delta: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    let mutable delta = delta
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Input arrays must have the same length."))
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let diff: float = (_idx y_true (int i)) - (_idx y_pred (int i))
            let adiff: float = absf (diff)
            if adiff <= delta then
                total <- total + ((0.5 * diff) * diff)
            else
                total <- total + (delta * (adiff - (0.5 * delta)))
            i <- i + 1
        __ret <- total / (to_float (Seq.length (y_true)))
        raise Return
        __ret
    with
        | Return -> __ret
and mean_squared_error (y_true: float array) (y_pred: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Input arrays must have the same length."))
        let mutable losses: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let diff: float = (_idx y_true (int i)) - (_idx y_pred (int i))
            losses <- Array.append losses [|(diff * diff)|]
            i <- i + 1
        __ret <- mean (losses)
        raise Return
        __ret
    with
        | Return -> __ret
and mean_absolute_error (y_true: float array) (y_pred: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Input arrays must have the same length."))
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            total <- total + (absf ((_idx y_true (int i)) - (_idx y_pred (int i))))
            i <- i + 1
        __ret <- total / (to_float (Seq.length (y_true)))
        raise Return
        __ret
    with
        | Return -> __ret
and mean_squared_logarithmic_error (y_true: float array) (y_pred: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Input arrays must have the same length."))
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let mutable a: float = ln (1.0 + (_idx y_true (int i)))
            let mutable b: float = ln (1.0 + (_idx y_pred (int i)))
            let diff: float = a - b
            total <- total + (diff * diff)
            i <- i + 1
        __ret <- total / (to_float (Seq.length (y_true)))
        raise Return
        __ret
    with
        | Return -> __ret
and mean_absolute_percentage_error (y_true: float array) (y_pred: float array) (epsilon: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    let mutable epsilon = epsilon
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("The length of the two arrays should be the same."))
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let mutable yt: float = _idx y_true (int i)
            if yt = 0.0 then
                yt <- epsilon
            total <- total + (absf ((yt - (_idx y_pred (int i))) / yt))
            i <- i + 1
        __ret <- total / (to_float (Seq.length (y_true)))
        raise Return
        __ret
    with
        | Return -> __ret
and perplexity_loss (y_true: int array array) (y_pred: float array array array) (epsilon: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    let mutable epsilon = epsilon
    try
        let batch: int = Seq.length (y_true)
        if batch <> (Seq.length (y_pred)) then
            ignore (failwith ("Batch size of y_true and y_pred must be equal."))
        let sentence_len: int = Seq.length (_idx y_true (int 0))
        if sentence_len <> (Seq.length (_idx y_pred (int 0))) then
            ignore (failwith ("Sentence length of y_true and y_pred must be equal."))
        let vocab_size: int = Seq.length (_idx (_idx y_pred (int 0)) (int 0))
        let mutable b: int = 0
        let mutable total_perp: float = 0.0
        while b < batch do
            if ((Seq.length (_idx y_true (int b))) <> sentence_len) || ((Seq.length (_idx y_pred (int b))) <> sentence_len) then
                ignore (failwith ("Sentence length of y_true and y_pred must be equal."))
            let mutable sum_log: float = 0.0
            let mutable j: int = 0
            while j < sentence_len do
                let label: int = _idx (_idx y_true (int b)) (int j)
                if label >= vocab_size then
                    ignore (failwith ("Label value must not be greater than vocabulary size."))
                let prob: float = clip (_idx (_idx (_idx y_pred (int b)) (int j)) (int label)) (epsilon) (1.0)
                sum_log <- sum_log + (ln (prob))
                j <- j + 1
            let mean_log: float = sum_log / (to_float (sentence_len))
            let perp: float = exp (-mean_log)
            total_perp <- total_perp + perp
            b <- b + 1
        __ret <- total_perp / (to_float (batch))
        raise Return
        __ret
    with
        | Return -> __ret
and smooth_l1_loss (y_true: float array) (y_pred: float array) (beta: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    let mutable beta = beta
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("The length of the two arrays should be the same."))
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            let diff: float = absf ((_idx y_true (int i)) - (_idx y_pred (int i)))
            if diff < beta then
                total <- total + (((0.5 * diff) * diff) / beta)
            else
                total <- (total + diff) - (0.5 * beta)
            i <- i + 1
        __ret <- total / (to_float (Seq.length (y_true)))
        raise Return
        __ret
    with
        | Return -> __ret
and kullback_leibler_divergence (y_true: float array) (y_pred: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y_true = y_true
    let mutable y_pred = y_pred
    try
        if (Seq.length (y_true)) <> (Seq.length (y_pred)) then
            ignore (failwith ("Input arrays must have the same length."))
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (y_true)) do
            total <- total + ((_idx y_true (int i)) * (ln ((_idx y_true (int i)) / (_idx y_pred (int i)))))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let y_true_bc: float array = unbox<float array> [|0.0; 1.0; 1.0; 0.0; 1.0|]
        let y_pred_bc: float array = unbox<float array> [|0.2; 0.7; 0.9; 0.3; 0.8|]
        ignore (printfn "%s" (_str (binary_cross_entropy (y_true_bc) (y_pred_bc) (0.000000000000001))))
        ignore (printfn "%s" (_str (binary_focal_cross_entropy (y_true_bc) (y_pred_bc) (2.0) (0.25) (0.000000000000001))))
        let y_true_cce: float array array = [|[|1.0; 0.0; 0.0|]; [|0.0; 1.0; 0.0|]; [|0.0; 0.0; 1.0|]|]
        let y_pred_cce: float array array = [|[|0.9; 0.1; 0.0|]; [|0.2; 0.7; 0.1|]; [|0.0; 0.1; 0.9|]|]
        ignore (printfn "%s" (_str (categorical_cross_entropy (y_true_cce) (y_pred_cce) (0.000000000000001))))
        let alpha: float array = unbox<float array> [|0.6; 0.2; 0.7|]
        ignore (printfn "%s" (_str (categorical_focal_cross_entropy (y_true_cce) (y_pred_cce) (alpha) (2.0) (0.000000000000001))))
        let y_true_hinge: float array = unbox<float array> [|-1.0; 1.0; 1.0; -1.0; 1.0|]
        let y_pred_hinge: float array = unbox<float array> [|-4.0; -0.3; 0.7; 5.0; 10.0|]
        ignore (printfn "%s" (_str (hinge_loss (y_true_hinge) (y_pred_hinge))))
        let y_true_huber: float array = unbox<float array> [|0.9; 10.0; 2.0; 1.0; 5.2|]
        let y_pred_huber: float array = unbox<float array> [|0.8; 2.1; 2.9; 4.2; 5.2|]
        ignore (printfn "%s" (_str (huber_loss (y_true_huber) (y_pred_huber) (1.0))))
        ignore (printfn "%s" (_str (mean_squared_error (y_true_huber) (y_pred_huber))))
        ignore (printfn "%s" (_str (mean_absolute_error (y_true_huber) (y_pred_huber))))
        ignore (printfn "%s" (_str (mean_squared_logarithmic_error (y_true_huber) (y_pred_huber))))
        let y_true_mape: float array = unbox<float array> [|10.0; 20.0; 30.0; 40.0|]
        let y_pred_mape: float array = unbox<float array> [|12.0; 18.0; 33.0; 45.0|]
        ignore (printfn "%s" (_str (mean_absolute_percentage_error (y_true_mape) (y_pred_mape) (0.000000000000001))))
        let y_true_perp: int array array = [|[|1; 4|]; [|2; 3|]|]
        let y_pred_perp: float array array array = [|[|[|0.28; 0.19; 0.21; 0.15; 0.17|]; [|0.24; 0.19; 0.09; 0.18; 0.3|]|]; [|[|0.03; 0.26; 0.21; 0.18; 0.32|]; [|0.28; 0.1; 0.33; 0.15; 0.14|]|]|]
        ignore (printfn "%s" (_str (perplexity_loss (y_true_perp) (y_pred_perp) (0.0000001))))
        let y_true_smooth: float array = unbox<float array> [|3.0; 5.0; 2.0; 7.0|]
        let y_pred_smooth: float array = unbox<float array> [|2.9; 4.8; 2.1; 7.2|]
        ignore (printfn "%s" (_str (smooth_l1_loss (y_true_smooth) (y_pred_smooth) (1.0))))
        let y_true_kl: float array = unbox<float array> [|0.2; 0.3; 0.5|]
        let y_pred_kl: float array = unbox<float array> [|0.3; 0.3; 0.4|]
        ignore (printfn "%s" (_str (kullback_leibler_divergence (y_true_kl) (y_pred_kl))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
