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
let rec int_to_float (x: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (float x) * 1.0
        raise Return
        __ret
    with
        | Return -> __ret
and abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (0.0 - x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and exp_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            term <- (term * x) / (int_to_float (i))
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and floor_int (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable i: int = 0
        while (int_to_float (i + 1)) <= x do
            i <- i + 1
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
and dot (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable s: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            s <- s + ((_idx a (i)) * (_idx b (i)))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and transpose (m: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable m = m
    try
        let rows: int = Seq.length (m)
        let cols: int = Seq.length (_idx m (0))
        let mutable res: float array array = Array.empty<float array>
        let mutable j: int = 0
        while j < cols do
            let mutable row: float array = Array.empty<float>
            let mutable i: int = 0
            while i < rows do
                row <- Array.append row [|(_idx (_idx m (i)) (j))|]
                i <- i + 1
            res <- Array.append res [|row|]
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and matmul (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let n: int = Seq.length (a)
        let m: int = Seq.length (_idx b (0))
        let p: int = Seq.length (b)
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < m do
                let mutable s: float = 0.0
                let mutable k: int = 0
                while k < p do
                    s <- s + ((_idx (_idx a (i)) (k)) * (_idx (_idx b (k)) (j)))
                    k <- k + 1
                row <- Array.append row [|s|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and matvec (a: float array array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            res <- Array.append res [|(dot (_idx a (i)) (b))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and identity (n: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable n = n
    try
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|(if i = j then 1.0 else 0.0)|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and invert (mat: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    try
        let n: int = Seq.length (mat)
        let mutable a: float array array = mat
        let mutable inv: float array array = identity (n)
        let mutable i: int = 0
        while i < n do
            let pivot: float = _idx (_idx a (i)) (i)
            let mutable j: int = 0
            while j < n do
                a.[i].[j] <- (_idx (_idx a (i)) (j)) / pivot
                inv.[i].[j] <- (_idx (_idx inv (i)) (j)) / pivot
                j <- j + 1
            let mutable k: int = 0
            while k < n do
                if k <> i then
                    let factor: float = _idx (_idx a (k)) (i)
                    j <- 0
                    while j < n do
                        a.[k].[j] <- (_idx (_idx a (k)) (j)) - (factor * (_idx (_idx a (i)) (j)))
                        inv.[k].[j] <- (_idx (_idx inv (k)) (j)) - (factor * (_idx (_idx inv (i)) (j)))
                        j <- j + 1
                k <- k + 1
            i <- i + 1
        __ret <- inv
        raise Return
        __ret
    with
        | Return -> __ret
and normal_equation (X: float array array) (y: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable X = X
    let mutable y = y
    try
        let Xt: float array array = transpose (X)
        let XtX: float array array = matmul (Xt) (X)
        let XtX_inv: float array array = invert (XtX)
        let Xty: float array = matvec (Xt) (y)
        __ret <- matvec (XtX_inv) (Xty)
        raise Return
        __ret
    with
        | Return -> __ret
and linear_regression_prediction (train_dt: float array) (train_usr: float array) (train_mtch: float array) (test_dt: float array) (test_mtch: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable train_dt = train_dt
    let mutable train_usr = train_usr
    let mutable train_mtch = train_mtch
    let mutable test_dt = test_dt
    let mutable test_mtch = test_mtch
    try
        let mutable X: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (train_dt)) do
            X <- Array.append X [|[|1.0; _idx train_dt (i); _idx train_mtch (i)|]|]
            i <- i + 1
        let beta: float array = normal_equation (X) (train_usr)
        __ret <- abs_float (((_idx beta (0)) + ((_idx test_dt (0)) * (_idx beta (1)))) + ((_idx test_mtch (0)) * (_idx beta (2))))
        raise Return
        __ret
    with
        | Return -> __ret
and sarimax_predictor (train_user: float array) (train_match: float array) (test_match: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable train_user = train_user
    let mutable train_match = train_match
    let mutable test_match = test_match
    try
        let n: int = Seq.length (train_user)
        let mutable X: float array array = Array.empty<float array>
        let mutable y: float array = Array.empty<float>
        let mutable i: int = 1
        while i < n do
            X <- Array.append X [|[|1.0; _idx train_user (i - 1); _idx train_match (i)|]|]
            y <- Array.append y [|(_idx train_user (i))|]
            i <- i + 1
        let beta: float array = normal_equation (X) (y)
        __ret <- ((_idx beta (0)) + ((_idx beta (1)) * (_idx train_user (n - 1)))) + ((_idx beta (2)) * (_idx test_match (0)))
        raise Return
        __ret
    with
        | Return -> __ret
and rbf_kernel (a: float array) (b: float array) (gamma: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    let mutable gamma = gamma
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let diff: float = (_idx a (i)) - (_idx b (i))
            sum <- sum + (diff * diff)
            i <- i + 1
        __ret <- exp_approx ((-gamma) * sum)
        raise Return
        __ret
    with
        | Return -> __ret
and support_vector_regressor (x_train: float array array) (x_test: float array array) (train_user: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x_train = x_train
    let mutable x_test = x_test
    let mutable train_user = train_user
    try
        let gamma: float = 0.1
        let mutable weights: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (x_train)) do
            weights <- Array.append weights [|(rbf_kernel (_idx x_train (i)) (_idx x_test (0)) (gamma))|]
            i <- i + 1
        let mutable num: float = 0.0
        let mutable den: float = 0.0
        i <- 0
        while i < (Seq.length (train_user)) do
            num <- num + ((_idx weights (i)) * (_idx train_user (i)))
            den <- den + (_idx weights (i))
            i <- i + 1
        __ret <- num / den
        raise Return
        __ret
    with
        | Return -> __ret
and set_at_float (xs: float array) (idx: int) (value: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable i: int = 0
        let mutable res: float array = Array.empty<float>
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|(_idx xs (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and sort_float (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable res: float array = xs
        let mutable i: int = 1
        while i < (Seq.length (res)) do
            let key: float = _idx res (i)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx res (j)) > key) do
                res <- set_at_float (res) (j + 1) (_idx res (j))
                j <- j - 1
            res <- set_at_float (res) (j + 1) (key)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and percentile (data: float array) (q: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable data = data
    let mutable q = q
    try
        let mutable sorted: float array = sort_float (data)
        let n: int = Seq.length (sorted)
        let pos: float = (q / 100.0) * (int_to_float (n - 1))
        let idx: int = floor_int (pos)
        let frac: float = pos - (int_to_float (idx))
        if (idx + 1) < n then
            __ret <- ((_idx sorted (idx)) * (1.0 - frac)) + ((_idx sorted (idx + 1)) * frac)
            raise Return
        __ret <- _idx sorted (idx)
        raise Return
        __ret
    with
        | Return -> __ret
and interquartile_range_checker (train_user: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable train_user = train_user
    try
        let q1: float = percentile (train_user) (25.0)
        let q3: float = percentile (train_user) (75.0)
        let iqr: float = q3 - q1
        __ret <- q1 - (iqr * 0.1)
        raise Return
        __ret
    with
        | Return -> __ret
and data_safety_checker (list_vote: float array) (actual_result: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable list_vote = list_vote
    let mutable actual_result = actual_result
    try
        let mutable safe: int = 0
        let mutable not_safe: int = 0
        let mutable i: int = 0
        while i < (Seq.length (list_vote)) do
            let v: float = _idx list_vote (i)
            if v > actual_result then
                safe <- not_safe + 1
            else
                if (abs_float ((abs_float (v)) - (abs_float (actual_result)))) <= 0.1 then
                    safe <- safe + 1
                else
                    not_safe <- not_safe + 1
            i <- i + 1
        __ret <- safe > not_safe
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let vote: float array = unbox<float array> [|linear_regression_prediction (unbox<float array> [|2.0; 3.0; 4.0; 5.0|]) (unbox<float array> [|5.0; 3.0; 4.0; 6.0|]) (unbox<float array> [|3.0; 1.0; 2.0; 4.0|]) (unbox<float array> [|2.0|]) (unbox<float array> [|2.0|]); sarimax_predictor (unbox<float array> [|4.0; 2.0; 6.0; 8.0|]) (unbox<float array> [|3.0; 1.0; 2.0; 4.0|]) (unbox<float array> [|2.0|]); support_vector_regressor ([|[|5.0; 2.0|]; [|1.0; 5.0|]; [|6.0; 2.0|]|]) ([|[|3.0; 2.0|]|]) (unbox<float array> [|2.0; 1.0; 4.0|])|]
        printfn "%g" (_idx vote (0))
        printfn "%g" (_idx vote (1))
        printfn "%g" (_idx vote (2))
        printfn "%b" (data_safety_checker (vote) (5.0))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
