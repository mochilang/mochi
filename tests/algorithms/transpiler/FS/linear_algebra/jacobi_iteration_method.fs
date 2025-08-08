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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec strictly_diagonally_dominant (matrix: float array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable matrix = matrix
    try
        let mutable n: int = Seq.length (matrix)
        let mutable i: int = 0
        while i < n do
            let mutable sum: float = 0.0
            let mutable j: int = 0
            while j < n do
                if i <> j then
                    sum <- sum + (absf (_idx (_idx matrix (i)) (j)))
                j <- j + 1
            if (absf (_idx (_idx matrix (i)) (i))) <= sum then
                failwith ("Coefficient matrix is not strictly diagonally dominant")
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec jacobi_iteration_method (coefficient: float array array) (constant: float array) (init_val: float array) (iterations: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable coefficient = coefficient
    let mutable constant = constant
    let mutable init_val = init_val
    let mutable iterations = iterations
    try
        let n: int = Seq.length (coefficient)
        if n = 0 then
            failwith ("Coefficient matrix cannot be empty")
        if (Seq.length (constant)) <> n then
            failwith ("Constant vector length must equal number of rows in coefficient matrix")
        if (Seq.length (init_val)) <> n then
            failwith ("Initial values count must match matrix size")
        let mutable r: int = 0
        while r < n do
            if (Seq.length (_idx coefficient (r))) <> n then
                failwith ("Coefficient matrix must be square")
            r <- r + 1
        if iterations <= 0 then
            failwith ("Iterations must be at least 1")
        strictly_diagonally_dominant (coefficient)
        let mutable x: float array = init_val
        let mutable k: int = 0
        while k < iterations do
            let mutable new_x: float array = Array.empty<float>
            let mutable i: int = 0
            while i < n do
                let mutable sum: float = 0.0
                let mutable j: int = 0
                while j < n do
                    if i <> j then
                        sum <- sum + ((_idx (_idx coefficient (i)) (j)) * (_idx x (j)))
                    j <- j + 1
                let mutable value: float = ((_idx constant (i)) - sum) / (_idx (_idx coefficient (i)) (i))
                new_x <- Array.append new_x [|value|]
                i <- i + 1
            x <- new_x
            k <- k + 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let coefficient: float array array = [|[|4.0; 1.0; 1.0|]; [|1.0; 5.0; 2.0|]; [|1.0; 2.0; 4.0|]|]
let constant: float array = unbox<float array> [|2.0; -6.0; -4.0|]
let init_val: float array = unbox<float array> [|0.5; -0.5; -0.5|]
let iterations: int = 3
let result: float array = jacobi_iteration_method (coefficient) (constant) (init_val) (iterations)
printfn "%s" (_repr (result))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
