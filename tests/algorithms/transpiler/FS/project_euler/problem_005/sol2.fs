// Generated 2025-08-23 14:49 +0700

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
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let rec gcd (a: int64) (b: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int64 = a
        let mutable y: int64 = b
        while y <> (int64 0) do
            let mutable t: int64 = y
            y <- ((x % y + y) % y)
            x <- t
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and lcm (x: int64) (y: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable x = x
    let mutable y = y
    try
        __ret <- _floordiv64 (int64 (x * y)) (int64 (gcd (x) (y)))
        raise Return
        __ret
    with
        | Return -> __ret
and solution (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable g: int64 = int64 1
        let mutable i: int64 = int64 1
        while i <= n do
            g <- lcm (g) (i)
            i <- i + (int64 1)
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (printfn "%s" (_str (solution (int64 20))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
