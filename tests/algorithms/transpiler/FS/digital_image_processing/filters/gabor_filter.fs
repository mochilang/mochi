// Generated 2025-08-07 15:46 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let rec to_radians (deg: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable deg = deg
    try
        __ret <- (deg * PI) / 180.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec sin_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable i: int = 1
        while i < 10 do
            let k1: float = 2.0 * (float i)
            let k2: float = k1 + 1.0
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec cos_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            let k1: float = (2.0 * (float i)) - 1.0
            let k2: float = 2.0 * (float i)
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec exp_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: float = 1.0
        while i < 20.0 do
            term <- (term * x) / i
            sum <- sum + term
            i <- i + 1.0
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec gabor_filter_kernel (ksize: int) (sigma: float) (theta: float) (lambd: float) (gamma: float) (psi: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable ksize = ksize
    let mutable sigma = sigma
    let mutable theta = theta
    let mutable lambd = lambd
    let mutable gamma = gamma
    let mutable psi = psi
    try
        let mutable size: int = ksize
        if (((size % 2 + 2) % 2)) = 0 then
            size <- size + 1
        let mutable gabor: float array array = [||]
        let mutable y: int = 0
        while y < size do
            let mutable row: float array = [||]
            let mutable x: int = 0
            while x < size do
                let px: float = float (x - (size / 2))
                let py: float = float (y - (size / 2))
                let rad: float = to_radians (theta)
                let cos_theta: float = cos_taylor (rad)
                let sin_theta: float = sin_taylor (rad)
                let x_rot: float = (cos_theta * px) + (sin_theta * py)
                let y_rot: float = ((-sin_theta) * px) + (cos_theta * py)
                let exponent: float = (-((x_rot * x_rot) + (((gamma * gamma) * y_rot) * y_rot))) / ((2.0 * sigma) * sigma)
                let value: float = (exp_taylor (exponent)) * (cos_taylor ((((2.0 * PI) * x_rot) / lambd) + psi))
                row <- Array.append row [|value|]
                x <- x + 1
            gabor <- Array.append gabor [|row|]
            y <- y + 1
        __ret <- gabor
        raise Return
        __ret
    with
        | Return -> __ret
let kernel: float array array = gabor_filter_kernel (3) (8.0) (0.0) (10.0) (0.0) (0.0)
printfn "%s" (_repr (kernel))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
