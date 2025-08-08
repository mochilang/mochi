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
let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and pow_int (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and nth_root (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        if x = 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            let mutable denom: float = pow_int (guess) (n - 1)
            guess <- (((float (n - 1)) * guess) + (x / denom)) / (float n)
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and round_nearest (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x >= 0.0 then
            let n: int = int (x + 0.5)
            __ret <- float n
            raise Return
        let n: int = int (x - 0.5)
        __ret <- float n
        raise Return
        __ret
    with
        | Return -> __ret
and compute_geometric_mean (nums: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            failwith ("no numbers")
        let mutable product: float = 1.0
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            product <- product * (_idx nums (i))
            i <- i + 1
        if (product < 0.0) && (((((Seq.length (nums)) % 2 + 2) % 2)) = 0) then
            failwith ("Cannot Compute Geometric Mean for these numbers.")
        let mutable mean: float = nth_root (abs (product)) (Seq.length (nums))
        if product < 0.0 then
            mean <- -mean
        let possible: float = round_nearest (mean)
        if (pow_int (possible) (Seq.length (nums))) = product then
            mean <- possible
        __ret <- mean
        raise Return
        __ret
    with
        | Return -> __ret
and test_compute_geometric_mean () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let eps: float = 0.0001
        let m1: float = compute_geometric_mean (unbox<float array> [|2.0; 8.0|])
        if (abs (m1 - 4.0)) > eps then
            failwith ("test1 failed")
        let m2: float = compute_geometric_mean (unbox<float array> [|5.0; 125.0|])
        if (abs (m2 - 25.0)) > eps then
            failwith ("test2 failed")
        let m3: float = compute_geometric_mean (unbox<float array> [|1.0; 0.0|])
        if (abs (m3 - 0.0)) > eps then
            failwith ("test3 failed")
        let m4: float = compute_geometric_mean (unbox<float array> [|1.0; 5.0; 25.0; 5.0|])
        if (abs (m4 - 5.0)) > eps then
            failwith ("test4 failed")
        let m5: float = compute_geometric_mean (unbox<float array> [|-5.0; 25.0; 1.0|])
        if (abs (m5 + 5.0)) > eps then
            failwith ("test5 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_compute_geometric_mean()
        printfn "%g" (compute_geometric_mean (unbox<float array> [|-3.0; -27.0|]))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
