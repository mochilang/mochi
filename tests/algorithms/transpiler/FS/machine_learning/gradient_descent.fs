// Generated 2025-08-08 17:07 +0700

exception Break
exception Continue

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
type DataPoint = {
    mutable _x: float array
    mutable _y: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec absf (_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _x = _x
    try
        __ret <- if _x < 0.0 then (-_x) else _x
        raise Return
        __ret
    with
        | Return -> __ret
let rec hypothesis_value (input: float array) (params: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable input = input
    let mutable params = params
    try
        let mutable value: float = _idx params (0)
        let mutable i: int = 0
        while i < (Seq.length (input)) do
            value <- value + ((_idx input (i)) * (_idx params (i + 1)))
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let rec calc_error (dp: DataPoint) (params: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable dp = dp
    let mutable params = params
    try
        __ret <- (hypothesis_value (dp._x) (params)) - (dp._y)
        raise Return
        __ret
    with
        | Return -> __ret
let rec summation_of_cost_derivative (index: int) (params: float array) (data: DataPoint array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable index = index
    let mutable params = params
    let mutable data = data
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (data)) do
            let dp: DataPoint = _idx data (i)
            let e: float = calc_error (dp) (params)
            if index = (-1) then
                sum <- sum + e
            else
                sum <- sum + (e * (_idx (dp._x) (index)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_cost_derivative (index: int) (params: float array) (data: DataPoint array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable index = index
    let mutable params = params
    let mutable data = data
    try
        __ret <- (summation_of_cost_derivative (index) (params) (data)) / (float (Seq.length (data)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec allclose (a: float array) (b: float array) (atol: float) (rtol: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable atol = atol
    let mutable rtol = rtol
    try
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let diff: float = absf ((_idx a (i)) - (_idx b (i)))
            let limit: float = atol + (rtol * (absf (_idx b (i))))
            if diff > limit then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec run_gradient_descent (train_data: DataPoint array) (initial_params: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable train_data = train_data
    let mutable initial_params = initial_params
    try
        let learning_rate: float = 0.009
        let absolute_error_limit: float = 0.000002
        let relative_error_limit: float = 0.0
        let mutable j: int = 0
        let mutable params: float array = initial_params
        try
            while true do
                try
                    j <- j + 1
                    let mutable temp: float array = Array.empty<float>
                    let mutable i: int = 0
                    while i < (Seq.length (params)) do
                        let deriv: float = get_cost_derivative (i - 1) (params) (train_data)
                        temp <- Array.append temp [|((_idx params (i)) - (learning_rate * deriv))|]
                        i <- i + 1
                    if allclose (params) (temp) (absolute_error_limit) (relative_error_limit) then
                        printfn "%s" ("Number of iterations:" + (_str (j)))
                        raise Break
                    params <- temp
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- params
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_gradient_descent (test_data: DataPoint array) (params: float array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable test_data = test_data
    let mutable params = params
    try
        let mutable i: int = 0
        while i < (Seq.length (test_data)) do
            let dp: DataPoint = _idx test_data (i)
            printfn "%s" ("Actual output value:" + (_str (dp._y)))
            printfn "%s" ("Hypothesis output:" + (_str (hypothesis_value (dp._x) (params))))
            i <- i + 1
        __ret
    with
        | Return -> __ret
let train_data: DataPoint array = unbox<DataPoint array> [|{ _x = unbox<float array> [|5.0; 2.0; 3.0|]; _y = 15.0 }; { _x = unbox<float array> [|6.0; 5.0; 9.0|]; _y = 25.0 }; { _x = unbox<float array> [|11.0; 12.0; 13.0|]; _y = 41.0 }; { _x = unbox<float array> [|1.0; 1.0; 1.0|]; _y = 8.0 }; { _x = unbox<float array> [|11.0; 12.0; 13.0|]; _y = 41.0 }|]
let test_data: DataPoint array = unbox<DataPoint array> [|{ _x = unbox<float array> [|515.0; 22.0; 13.0|]; _y = 555.0 }; { _x = unbox<float array> [|61.0; 35.0; 49.0|]; _y = 150.0 }|]
let mutable parameter_vector: float array = unbox<float array> [|2.0; 4.0; 1.0; 5.0|]
parameter_vector <- run_gradient_descent (train_data) (parameter_vector)
printfn "%s" ("\nTesting gradient descent for a linear hypothesis function.\n")
test_gradient_descent (test_data) (parameter_vector)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
