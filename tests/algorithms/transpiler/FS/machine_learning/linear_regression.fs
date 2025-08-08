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
let rec dot (x: float array) (y: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (x)) do
            sum <- sum + ((_idx x (i)) * (_idx y (i)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec run_steep_gradient_descent (data_x: float array array) (data_y: float array) (len_data: int) (alpha: float) (theta: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable data_x = data_x
    let mutable data_y = data_y
    let mutable len_data = len_data
    let mutable alpha = alpha
    let mutable theta = theta
    try
        let mutable gradients: float array = Array.empty<float>
        let mutable j: int = 0
        while j < (Seq.length (theta)) do
            gradients <- Array.append gradients [|0.0|]
            j <- j + 1
        let mutable i: int = 0
        while i < len_data do
            let prediction: float = dot (theta) (_idx data_x (i))
            let error: float = prediction - (_idx data_y (i))
            let mutable k: int = 0
            while k < (Seq.length (theta)) do
                gradients.[k] <- (_idx gradients (k)) + (error * (_idx (_idx data_x (i)) (k)))
                k <- k + 1
            i <- i + 1
        let mutable t: float array = Array.empty<float>
        let mutable g: int = 0
        while g < (Seq.length (theta)) do
            t <- Array.append t [|((_idx theta (g)) - ((alpha / (float len_data)) * (_idx gradients (g))))|]
            g <- g + 1
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
let rec sum_of_square_error (data_x: float array array) (data_y: float array) (len_data: int) (theta: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable data_x = data_x
    let mutable data_y = data_y
    let mutable len_data = len_data
    let mutable theta = theta
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < len_data do
            let prediction: float = dot (theta) (_idx data_x (i))
            let diff: float = prediction - (_idx data_y (i))
            total <- total + (diff * diff)
            i <- i + 1
        __ret <- total / (2.0 * (float len_data))
        raise Return
        __ret
    with
        | Return -> __ret
let rec run_linear_regression (data_x: float array array) (data_y: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable data_x = data_x
    let mutable data_y = data_y
    try
        let iterations: int = 10
        let alpha: float = 0.01
        let no_features: int = Seq.length (_idx data_x (0))
        let len_data: int = Seq.length (data_x)
        let mutable theta: float array = Array.empty<float>
        let mutable i: int = 0
        while i < no_features do
            theta <- Array.append theta [|0.0|]
            i <- i + 1
        let mutable iter: int = 0
        while iter < iterations do
            theta <- run_steep_gradient_descent (data_x) (data_y) (len_data) (alpha) (theta)
            let error: float = sum_of_square_error (data_x) (data_y) (len_data) (theta)
            printfn "%s" ((("At Iteration " + (_str (iter + 1))) + " - Error is ") + (_str (error)))
            iter <- iter + 1
        __ret <- theta
        raise Return
        __ret
    with
        | Return -> __ret
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x < 0.0 then
            __ret <- -x
            raise Return
        else
            __ret <- x
            raise Return
        __ret
    with
        | Return -> __ret
let rec mean_absolute_error (predicted_y: float array) (original_y: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable predicted_y = predicted_y
    let mutable original_y = original_y
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (predicted_y)) do
            let diff: float = absf ((_idx predicted_y (i)) - (_idx original_y (i)))
            total <- total + diff
            i <- i + 1
        __ret <- total / (float (Seq.length (predicted_y)))
        raise Return
        __ret
    with
        | Return -> __ret
let data_x: float array array = [|[|1.0; 1.0|]; [|1.0; 2.0|]; [|1.0; 3.0|]|]
let data_y: float array = unbox<float array> [|1.0; 2.0; 3.0|]
let mutable theta: float array = run_linear_regression (data_x) (data_y)
printfn "%s" ("Resultant Feature vector :")
let mutable i: int = 0
while i < (Seq.length (theta)) do
    printfn "%s" (_str (_idx theta (i)))
    i <- i + 1
let predicted_y: float array = unbox<float array> [|3.0; -0.5; 2.0; 7.0|]
let original_y: float array = unbox<float array> [|2.5; 0.0; 2.0; 8.0|]
let mae: float = mean_absolute_error (predicted_y) (original_y)
printfn "%s" ("Mean Absolute Error : " + (_str (mae)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
