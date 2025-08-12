// Generated 2025-08-12 07:47 +0700

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
let PI: float = 3.141592653589793
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x < 0.0 then
            failwith ("sqrt domain error")
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
and ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            failwith ("ln domain error")
        let y: float = (x - 1.0) / (x + 1.0)
        let y2: float = y * y
        let mutable term: float = y
        let mutable sum: float = 0.0
        let mutable k: int = 0
        while k < 10 do
            let denom: float = float (((int64 2) * (int64 k)) + (int64 1))
            sum <- sum + (term / denom)
            term <- term * y2
            k <- k + 1
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
and exp_series (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n < 20 do
            term <- (term * x) / (float n)
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and powf (``base``: float) (exponent: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    try
        __ret <- if ``base`` <= 0.0 then 0.0 else (exp_series (exponent * (ln (``base``))))
        raise Return
        __ret
    with
        | Return -> __ret
and integrand (x: float) (z: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable z = z
    try
        __ret <- (powf (x) (z - 1.0)) * (exp_series (-x))
        raise Return
        __ret
    with
        | Return -> __ret
and gamma_iterative (num: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable num = num
    try
        if num <= 0.0 then
            failwith ("math domain error")
        let step: float = 0.001
        let limit: float = 100.0
        let mutable x: float = step
        let mutable total: float = 0.0
        while x < limit do
            total <- total + ((integrand (x) (num)) * step)
            x <- x + step
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and gamma_recursive (num: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable num = num
    try
        if num <= 0.0 then
            failwith ("math domain error")
        if num > 171.5 then
            failwith ("math range error")
        let int_part: int = int (num)
        let frac: float = num - (float int_part)
        if not (((absf (frac)) < 0.000001) || ((absf (frac - 0.5)) < 0.000001)) then
            failwith ("num must be an integer or a half-integer")
        if (absf (num - 0.5)) < 0.000001 then
            __ret <- sqrt (PI)
            raise Return
        if (absf (num - 1.0)) < 0.000001 then
            __ret <- 1.0
            raise Return
        __ret <- (num - 1.0) * (gamma_recursive (num - 1.0))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%g" (gamma_iterative (5.0))
        printfn "%g" (gamma_recursive (5.0))
        printfn "%g" (gamma_recursive (0.5))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
