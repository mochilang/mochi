// Generated 2025-08-07 16:27 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec n_choose_k (n: int) (k: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    let mutable k = k
    try
        if (k < 0) || (k > n) then
            __ret <- 0.0
            raise Return
        if (k = 0) || (k = n) then
            __ret <- 1.0
            raise Return
        let mutable result: float = 1.0
        let mutable i: int = 1
        while i <= k do
            result <- (result * (1.0 * (float ((n - k) + i)))) / (1.0 * (float i))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow_float (``base``: float) (exp: int) =
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
let rec basis_function (points: float array array) (t: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable points = points
    let mutable t = t
    try
        let degree: int = (Seq.length (points)) - 1
        let mutable res: float array = [||]
        let mutable i: int = 0
        while i <= degree do
            let coef: float = n_choose_k (degree) (i)
            let term: float = (pow_float (1.0 - t) (degree - i)) * (pow_float (t) (i))
            res <- Array.append res [|(coef * term)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bezier_point (points: float array array) (t: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable points = points
    let mutable t = t
    try
        let basis: float array = basis_function (points) (t)
        let mutable x: float = 0.0
        let mutable y: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (points)) do
            x <- x + ((_idx basis (i)) * (_idx (_idx points (i)) (0)))
            y <- y + ((_idx basis (i)) * (_idx (_idx points (i)) (1)))
            i <- i + 1
        __ret <- unbox<float array> [|x; y|]
        raise Return
        __ret
    with
        | Return -> __ret
let control: float array array = [|[|1.0; 1.0|]; [|1.0; 2.0|]|]
printfn "%s" (_str (basis_function (control) (0.0)))
printfn "%s" (_str (basis_function (control) (1.0)))
printfn "%s" (_str (bezier_point (control) (0.0)))
printfn "%s" (_str (bezier_point (control) (1.0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
