// Generated 2025-08-08 17:35 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec powf (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let mutable res: float = 1.0
        let mutable i: int = 0
        while i < n do
            res <- res * x
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec roundf (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x >= 0.0 then (int (x + 0.5)) else (int (x - 0.5))
        raise Return
        __ret
    with
        | Return -> __ret
let rec fib_iterative (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 0 then
            failwith ("n is negative")
        if n = 0 then
            __ret <- unbox<int array> [|0|]
            raise Return
        let mutable fib: int array = unbox<int array> [|0; 1|]
        let mutable i: int = 2
        while i <= n do
            fib <- Array.append fib [|((_idx fib (i - 1)) + (_idx fib (i - 2)))|]
            i <- i + 1
        __ret <- fib
        raise Return
        __ret
    with
        | Return -> __ret
let rec fib_recursive_term (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        if i < 0 then
            failwith ("n is negative")
        if i < 2 then
            __ret <- i
            raise Return
        __ret <- (fib_recursive_term (i - 1)) + (fib_recursive_term (i - 2))
        raise Return
        __ret
    with
        | Return -> __ret
let rec fib_recursive (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 0 then
            failwith ("n is negative")
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= n do
            res <- Array.append res [|(fib_recursive_term (i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let mutable fib_cache_global: System.Collections.Generic.IDictionary<int, int> = _dictCreate []
let rec fib_recursive_cached_term (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        if i < 0 then
            failwith ("n is negative")
        if i < 2 then
            __ret <- i
            raise Return
        if fib_cache_global.ContainsKey(i) then
            __ret <- _dictGet fib_cache_global (i)
            raise Return
        let ``val``: int = (fib_recursive_cached_term (i - 1)) + (fib_recursive_cached_term (i - 2))
        fib_cache_global.[i] <- ``val``
        __ret <- ``val``
        raise Return
        __ret
    with
        | Return -> __ret
let rec fib_recursive_cached (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 0 then
            failwith ("n is negative")
        let mutable res: int array = Array.empty<int>
        let mutable j: int = 0
        while j <= n do
            res <- Array.append res [|(fib_recursive_cached_term (j))|]
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let mutable fib_memo_cache: System.Collections.Generic.IDictionary<int, int> = unbox<System.Collections.Generic.IDictionary<int, int>> (_dictCreate [(0, 0); (1, 1); (2, 1)])
let rec fib_memoization_term (num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    try
        if fib_memo_cache.ContainsKey(num) then
            __ret <- _dictGet fib_memo_cache (num)
            raise Return
        let value: int = (fib_memoization_term (num - 1)) + (fib_memoization_term (num - 2))
        fib_memo_cache.[num] <- value
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let rec fib_memoization (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 0 then
            failwith ("n is negative")
        let mutable out: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= n do
            out <- Array.append out [|(fib_memoization_term (i))|]
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec fib_binet (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 0 then
            failwith ("n is negative")
        if n >= 1475 then
            failwith ("n is too large")
        let sqrt5: float = sqrt (5.0)
        let phi: float = (1.0 + sqrt5) / 2.0
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= n do
            let ``val``: int = roundf ((powf (phi) (i)) / sqrt5)
            res <- Array.append res [|``val``|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_mul (a: int array array) (b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    try
        let a00: int = ((_idx (_idx a (0)) (0)) * (_idx (_idx b (0)) (0))) + ((_idx (_idx a (0)) (1)) * (_idx (_idx b (1)) (0)))
        let a01: int = ((_idx (_idx a (0)) (0)) * (_idx (_idx b (0)) (1))) + ((_idx (_idx a (0)) (1)) * (_idx (_idx b (1)) (1)))
        let a10: int = ((_idx (_idx a (1)) (0)) * (_idx (_idx b (0)) (0))) + ((_idx (_idx a (1)) (1)) * (_idx (_idx b (1)) (0)))
        let a11: int = ((_idx (_idx a (1)) (0)) * (_idx (_idx b (0)) (1))) + ((_idx (_idx a (1)) (1)) * (_idx (_idx b (1)) (1)))
        __ret <- [|[|a00; a01|]; [|a10; a11|]|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_pow (m: int array array) (power: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable m = m
    let mutable power = power
    try
        if power < 0 then
            failwith ("power is negative")
        let mutable result: int array array = [|[|1; 0|]; [|0; 1|]|]
        let mutable ``base``: int array array = m
        let mutable p: int = power
        while p > 0 do
            if (((p % 2 + 2) % 2)) = 1 then
                result <- matrix_mul (result) (``base``)
            ``base`` <- matrix_mul (``base``) (``base``)
            p <- int (_floordiv p 2)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec fib_matrix (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            failwith ("n is negative")
        if n = 0 then
            __ret <- 0
            raise Return
        let m: int array array = [|[|1; 1|]; [|1; 0|]|]
        let mutable res: int array array = matrix_pow (m) (n - 1)
        __ret <- _idx (_idx res (0)) (0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec run_tests () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let expected: int array = unbox<int array> [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55|]
        let it: int array = fib_iterative (10)
        let ``rec``: int array = fib_recursive (10)
        let cache: int array = fib_recursive_cached (10)
        let memo: int array = fib_memoization (10)
        let bin: int array = fib_binet (10)
        let m: int = fib_matrix (10)
        if it <> expected then
            failwith ("iterative failed")
        if ``rec`` <> expected then
            failwith ("recursive failed")
        if cache <> expected then
            failwith ("cached failed")
        if memo <> expected then
            failwith ("memoization failed")
        if bin <> expected then
            failwith ("binet failed")
        if m <> 55 then
            failwith ("matrix failed")
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (run_tests()))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
