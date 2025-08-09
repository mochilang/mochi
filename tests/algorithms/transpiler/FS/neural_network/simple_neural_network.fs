// Generated 2025-08-09 10:14 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((((int64 _seed) * (int64 1103515245)) + (int64 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
let rec randint (low: int) (high: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable low = low
    let mutable high = high
    try
        __ret <- ((((rand()) % ((high - low) + 1) + ((high - low) + 1)) % ((high - low) + 1))) + low
        raise Return
        __ret
    with
        | Return -> __ret
let rec expApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = x
        let mutable is_neg: bool = false
        if x < 0.0 then
            is_neg <- true
            y <- -x
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n < 30 do
            term <- (term * y) / (float n)
            sum <- sum + term
            n <- n + 1
        if is_neg then
            __ret <- 1.0 / sum
            raise Return
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec sigmoid (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- 1.0 / (1.0 + (expApprox (-x)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec sigmoid_derivative (sig_val: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable sig_val = sig_val
    try
        __ret <- sig_val * (1.0 - sig_val)
        raise Return
        __ret
    with
        | Return -> __ret
let INITIAL_VALUE: float = 0.02
let rec forward_propagation (expected: int) (number_propagations: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable expected = expected
    let mutable number_propagations = number_propagations
    try
        let mutable weight: float = (2.0 * (float (randint (1) (100)))) - 1.0
        let mutable layer_1: float = 0.0
        let mutable i: int = 0
        while i < number_propagations do
            layer_1 <- sigmoid (INITIAL_VALUE * weight)
            let layer_1_error: float = ((float expected) / 100.0) - layer_1
            let layer_1_delta: float = layer_1_error * (sigmoid_derivative (layer_1))
            weight <- weight + (INITIAL_VALUE * layer_1_delta)
            i <- i + 1
        __ret <- layer_1 * 100.0
        raise Return
        __ret
    with
        | Return -> __ret
_seed <- 1
let result: float = forward_propagation (32) (450000)
printfn "%g" (result)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
