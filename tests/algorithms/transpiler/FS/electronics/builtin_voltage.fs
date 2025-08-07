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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < n do
            result <- result * 10.0
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let BOLTZMANN: float = 1.380649 / (pow10 (23))
let ELECTRON_VOLT: float = 1.602176634 / (pow10 (19))
let TEMPERATURE: float = 300.0
let rec ln_series (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let t: float = (x - 1.0) / (x + 1.0)
        let mutable term: float = t
        let mutable sum: float = 0.0
        let mutable n: int = 1
        while n <= 19 do
            sum <- sum + (term / (float n))
            term <- (term * t) * t
            n <- n + 2
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = x
        let mutable k: int = 0
        while y >= 10.0 do
            y <- y / 10.0
            k <- k + 1
        while y < 1.0 do
            y <- y * 10.0
            k <- k - 1
        __ret <- (ln_series (y)) + ((float k) * (ln_series (10.0)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec builtin_voltage (donor_conc: float) (acceptor_conc: float) (intrinsic_conc: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable donor_conc = donor_conc
    let mutable acceptor_conc = acceptor_conc
    let mutable intrinsic_conc = intrinsic_conc
    try
        if donor_conc <= 0.0 then
            failwith ("Donor concentration should be positive")
        if acceptor_conc <= 0.0 then
            failwith ("Acceptor concentration should be positive")
        if intrinsic_conc <= 0.0 then
            failwith ("Intrinsic concentration should be positive")
        if donor_conc <= intrinsic_conc then
            failwith ("Donor concentration should be greater than intrinsic concentration")
        if acceptor_conc <= intrinsic_conc then
            failwith ("Acceptor concentration should be greater than intrinsic concentration")
        __ret <- ((BOLTZMANN * TEMPERATURE) * (ln ((donor_conc * acceptor_conc) / (intrinsic_conc * intrinsic_conc)))) / ELECTRON_VOLT
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (builtin_voltage (pow10 (17)) (pow10 (17)) (pow10 (10))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
