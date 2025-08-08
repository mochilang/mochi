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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
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
let rec sigmoid (z: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable z = z
    try
        __ret <- 1.0 / (1.0 + (expApprox (-z)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec dot (a: float array) (b: float array) =
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
let rec zeros (n: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable n = n
    try
        let mutable res: float array = Array.empty<float>
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|0.0|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec logistic_reg (alpha: float) (x: float array array) (y: float array) (iterations: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable alpha = alpha
    let mutable x = x
    let mutable y = y
    let mutable iterations = iterations
    try
        let m: int = Seq.length (x)
        let mutable n: int = Seq.length (_idx x (0))
        let mutable theta: float array = zeros (n)
        let mutable iter: int = 0
        while iter < iterations do
            let mutable grad: float array = zeros (n)
            let mutable i: int = 0
            while i < m do
                let z: float = dot (_idx x (i)) (theta)
                let h: float = sigmoid (z)
                let mutable k: int = 0
                while k < n do
                    grad.[k] <- (_idx grad (k)) + ((h - (_idx y (i))) * (_idx (_idx x (i)) (k)))
                    k <- k + 1
                i <- i + 1
            let mutable k2: int = 0
            while k2 < n do
                theta <- _arrset theta (k2) ((_idx theta (k2)) - ((alpha * (_idx grad (k2))) / (float m)))
                k2 <- k2 + 1
            iter <- iter + 1
        __ret <- theta
        raise Return
        __ret
    with
        | Return -> __ret
let x: float array array = [|[|0.5; 1.5|]; [|1.0; 1.0|]; [|1.5; 0.5|]; [|3.0; 3.5|]; [|3.5; 3.0|]; [|4.0; 4.0|]|]
let mutable y: float array = unbox<float array> [|0.0; 0.0; 0.0; 1.0; 1.0; 1.0|]
let alpha: float = 0.1
let iterations: int = 1000
let theta: float array = logistic_reg (alpha) (x) (y) (iterations)
for i in 0 .. ((Seq.length (theta)) - 1) do
    printfn "%g" (_idx theta (i))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
