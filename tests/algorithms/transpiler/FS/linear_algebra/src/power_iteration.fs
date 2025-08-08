// Generated 2025-08-08 16:34 +0700

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
type PowerResult = {
    mutable _eigenvalue: float
    mutable _eigenvector: float array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x = 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec dot (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            sum <- sum + ((_idx a (i)) * (_idx b (i)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec mat_vec_mult (mat: float array array) (vec: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable mat = mat
    let mutable vec = vec
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            res <- Array.append res [|(dot (_idx mat (i)) (vec))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec norm (vec: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable vec = vec
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (vec)) do
            sum <- sum + ((_idx vec (i)) * (_idx vec (i)))
            i <- i + 1
        let mutable root: float = sqrtApprox (sum)
        __ret <- root
        raise Return
        __ret
    with
        | Return -> __ret
let rec normalize (vec: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vec = vec
    try
        let n: float = norm (vec)
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (vec)) do
            res <- Array.append res [|((_idx vec (i)) / n)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec power_iteration (matrix: float array array) (vector: float array) (error_tol: float) (max_iterations: int) =
    let mutable __ret : PowerResult = Unchecked.defaultof<PowerResult>
    let mutable matrix = matrix
    let mutable vector = vector
    let mutable error_tol = error_tol
    let mutable max_iterations = max_iterations
    try
        let mutable v: float array = normalize (vector)
        let mutable lambda_prev: float = 0.0
        let mutable lambda: float = 0.0
        let mutable err: float = 1000000000000.0
        let mutable iterations: int = 0
        while (err > error_tol) && (iterations < max_iterations) do
            let w: float array = mat_vec_mult (matrix) (v)
            v <- normalize (w)
            let mv: float array = mat_vec_mult (matrix) (v)
            lambda <- dot (v) (mv)
            let denom: float = if lambda <> 0.0 then (abs (lambda)) else 1.0
            err <- (abs (lambda - lambda_prev)) / denom
            lambda_prev <- lambda
            iterations <- iterations + 1
        __ret <- { _eigenvalue = lambda; _eigenvector = v }
        raise Return
        __ret
    with
        | Return -> __ret
let input_matrix: float array array = [|[|41.0; 4.0; 20.0|]; [|4.0; 26.0; 30.0|]; [|20.0; 30.0; 50.0|]|]
let vector: float array = unbox<float array> [|41.0; 4.0; 20.0|]
let result: PowerResult = power_iteration (input_matrix) (vector) (0.000000000001) (100)
printfn "%s" (_str (result._eigenvalue))
printfn "%s" (_str (result._eigenvector))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
