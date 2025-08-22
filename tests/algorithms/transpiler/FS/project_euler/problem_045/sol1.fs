// Generated 2025-08-22 15:25 +0700

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
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec to_float (x: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (float x) * 1.0
        raise Return
        __ret
    with
        | Return -> __ret
and sqrt (x: float) =
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
and floor (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable n: int = 0
        let mutable y: float = x
        while y >= 1.0 do
            y <- y - 1.0
            n <- n + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and hexagonal_num (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- int ((int64 n) * (((int64 2) * (int64 n)) - (int64 1)))
        raise Return
        __ret
    with
        | Return -> __ret
and is_pentagonal (n: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable n = n
    try
        let root: float = sqrt (1.0 + (24.0 * (to_float (n))))
        let ``val``: float = (1.0 + root) / 6.0
        __ret <- abs(``val`` - (to_float (floor (``val``)))) < 1e-9
        raise Return
        __ret
    with
        | Return -> __ret
and solution (start: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable start = start
    try
        let mutable idx: int = start
        let mutable num: int = hexagonal_num (idx)
        while not (is_pentagonal (num)) do
            idx <- idx + 1
            num <- hexagonal_num (idx)
        __ret <- num
        raise Return
        __ret
    with
        | Return -> __ret
and test_hexagonal_num () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (hexagonal_num (143)) <> 40755 then
            ignore (failwith ("hexagonal_num(143) failed"))
        if (hexagonal_num (21)) <> 861 then
            ignore (failwith ("hexagonal_num(21) failed"))
        if (hexagonal_num (10)) <> 190 then
            ignore (failwith ("hexagonal_num(10) failed"))
        __ret
    with
        | Return -> __ret
and test_is_pentagonal () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if not (is_pentagonal (330)) then
            ignore (failwith ("330 should be pentagonal"))
        if is_pentagonal (7683) then
            ignore (failwith ("7683 should not be pentagonal"))
        if not (is_pentagonal (2380)) then
            ignore (failwith ("2380 should be pentagonal"))
        __ret
    with
        | Return -> __ret
and test_solution () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (solution (144)) <> 1533776805 then
            ignore (failwith ("solution failed"))
        __ret
    with
        | Return -> __ret
ignore (test_hexagonal_num())
ignore (test_is_pentagonal())
ignore (test_solution())
ignore (printfn "%s" ((_str (solution (144))) + " = "))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
