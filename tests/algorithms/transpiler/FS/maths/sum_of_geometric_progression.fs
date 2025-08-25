// Generated 2025-08-25 22:27 +0700

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
    match box v with
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
let rec pow_float (``base``: float) (exp: int64) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable exponent: int64 = exp
        if exponent < (int64 0) then
            exponent <- -exponent
            let mutable i: int64 = int64 0
            while i < exponent do
                result <- result * ``base``
                i <- i + (int64 1)
            __ret <- 1.0 / result
            raise Return
        let mutable i: int64 = int64 0
        while i < exponent do
            result <- result * ``base``
            i <- i + (int64 1)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_geometric_progression (first_term: int64) (common_ratio: int64) (num_of_terms: int64) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable first_term = first_term
    let mutable common_ratio = common_ratio
    let mutable num_of_terms = num_of_terms
    try
        if common_ratio = (int64 1) then
            __ret <- float (num_of_terms * first_term)
            raise Return
        let a: float = float first_term
        let r: float = float common_ratio
        __ret <- (a / (1.0 - r)) * (1.0 - (pow_float (r) (int64 num_of_terms)))
        raise Return
        __ret
    with
        | Return -> __ret
and test_sum () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if abs((sum_of_geometric_progression (int64 1) (int64 2) (int64 10)) - 1023.0) >= 1e-9 then
            ignore (failwith ("example1 failed"))
        if abs((sum_of_geometric_progression (int64 1) (int64 10) (int64 5)) - 11111.0) >= 1e-9 then
            ignore (failwith ("example2 failed"))
        if abs((sum_of_geometric_progression (int64 (-1)) (int64 2) (int64 10)) - (-1023.0)) >= 1e-9 then
            ignore (failwith ("example3 failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_sum())
        ignore (printfn "%s" (_str (sum_of_geometric_progression (int64 1) (int64 2) (int64 10))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
