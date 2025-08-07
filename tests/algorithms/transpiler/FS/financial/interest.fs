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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec panic (msg: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    try
        printfn "%s" (msg)
        __ret
    with
        | Return -> __ret
and powf (``base``: float) (exp: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < (int exp) do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and simple_interest (principal: float) (daily_rate: float) (days: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable principal = principal
    let mutable daily_rate = daily_rate
    let mutable days = days
    try
        if days <= 0.0 then
            panic ("days_between_payments must be > 0")
            __ret <- 0.0
            raise Return
        if daily_rate < 0.0 then
            panic ("daily_interest_rate must be >= 0")
            __ret <- 0.0
            raise Return
        if principal <= 0.0 then
            panic ("principal must be > 0")
            __ret <- 0.0
            raise Return
        __ret <- (principal * daily_rate) * days
        raise Return
        __ret
    with
        | Return -> __ret
and compound_interest (principal: float) (nominal_rate: float) (periods: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable principal = principal
    let mutable nominal_rate = nominal_rate
    let mutable periods = periods
    try
        if periods <= 0.0 then
            panic ("number_of_compounding_periods must be > 0")
            __ret <- 0.0
            raise Return
        if nominal_rate < 0.0 then
            panic ("nominal_annual_interest_rate_percentage must be >= 0")
            __ret <- 0.0
            raise Return
        if principal <= 0.0 then
            panic ("principal must be > 0")
            __ret <- 0.0
            raise Return
        __ret <- principal * ((powf (1.0 + nominal_rate) (periods)) - 1.0)
        raise Return
        __ret
    with
        | Return -> __ret
and apr_interest (principal: float) (apr: float) (years: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable principal = principal
    let mutable apr = apr
    let mutable years = years
    try
        if years <= 0.0 then
            panic ("number_of_years must be > 0")
            __ret <- 0.0
            raise Return
        if apr < 0.0 then
            panic ("nominal_annual_percentage_rate must be >= 0")
            __ret <- 0.0
            raise Return
        if principal <= 0.0 then
            panic ("principal must be > 0")
            __ret <- 0.0
            raise Return
        __ret <- compound_interest (principal) (apr / 365.0) (years * 365.0)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (simple_interest (18000.0) (0.06) (3.0)))
        printfn "%s" (_str (simple_interest (0.5) (0.06) (3.0)))
        printfn "%s" (_str (simple_interest (18000.0) (0.01) (10.0)))
        printfn "%s" (_str (compound_interest (10000.0) (0.05) (3.0)))
        printfn "%s" (_str (compound_interest (10000.0) (0.05) (1.0)))
        printfn "%s" (_str (apr_interest (10000.0) (0.05) (3.0)))
        printfn "%s" (_str (apr_interest (10000.0) (0.05) (1.0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
