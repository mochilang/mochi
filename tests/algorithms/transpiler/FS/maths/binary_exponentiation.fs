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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec binary_exp_recursive (``base``: float) (exponent: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    try
        if exponent < 0 then
            failwith ("exponent must be non-negative")
        if exponent = 0 then
            __ret <- 1.0
            raise Return
        if (((exponent % 2 + 2) % 2)) = 1 then
            __ret <- (binary_exp_recursive (``base``) (exponent - 1)) * ``base``
            raise Return
        let half: float = binary_exp_recursive (``base``) (_floordiv exponent 2)
        __ret <- half * half
        raise Return
        __ret
    with
        | Return -> __ret
let rec binary_exp_iterative (``base``: float) (exponent: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    try
        if exponent < 0 then
            failwith ("exponent must be non-negative")
        let mutable result: float = 1.0
        let mutable b: float = ``base``
        let mutable e: int = exponent
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- result * b
            b <- b * b
            e <- _floordiv e 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec binary_exp_mod_recursive (``base``: int) (exponent: int) (modulus: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    let mutable modulus = modulus
    try
        if exponent < 0 then
            failwith ("exponent must be non-negative")
        if modulus <= 0 then
            failwith ("modulus must be positive")
        if exponent = 0 then
            __ret <- ((1 % modulus + modulus) % modulus)
            raise Return
        if (((exponent % 2 + 2) % 2)) = 1 then
            __ret <- ((((binary_exp_mod_recursive (``base``) (exponent - 1) (modulus)) * (((``base`` % modulus + modulus) % modulus))) % modulus + modulus) % modulus)
            raise Return
        let r: int = binary_exp_mod_recursive (``base``) (_floordiv exponent 2) (modulus)
        __ret <- (((r * r) % modulus + modulus) % modulus)
        raise Return
        __ret
    with
        | Return -> __ret
let rec binary_exp_mod_iterative (``base``: int) (exponent: int) (modulus: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    let mutable modulus = modulus
    try
        if exponent < 0 then
            failwith ("exponent must be non-negative")
        if modulus <= 0 then
            failwith ("modulus must be positive")
        let mutable result: int = ((1 % modulus + modulus) % modulus)
        let mutable b: int = ((``base`` % modulus + modulus) % modulus)
        let mutable e: int = exponent
        while e > 0 do
            if (((e % 2 + 2) % 2)) = 1 then
                result <- (((result * b) % modulus + modulus) % modulus)
            b <- (((b * b) % modulus + modulus) % modulus)
            e <- _floordiv e 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (binary_exp_recursive (3.0) (5))
printfn "%g" (binary_exp_iterative (1.5) (4))
printfn "%d" (binary_exp_mod_recursive (3) (4) (5))
printfn "%d" (binary_exp_mod_iterative (11) (13) (7))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
