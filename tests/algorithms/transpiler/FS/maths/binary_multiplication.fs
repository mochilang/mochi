// Generated 2025-08-16 14:41 +0700

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
    | :? float as f -> sprintf "%.15g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec binary_multiply (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable res: int = 0
        while y > 0 do
            if (((y % 2 + 2) % 2)) = 1 then
                res <- res + x
            x <- x + x
            y <- int (_floordiv (int y) (int 2))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and binary_mod_multiply (a: int) (b: int) (modulus: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    let mutable modulus = modulus
    try
        let mutable x: int = a
        let mutable y: int = b
        let mutable res: int = 0
        while y > 0 do
            if (((y % 2 + 2) % 2)) = 1 then
                res <- ((((((res % modulus + modulus) % modulus)) + (((x % modulus + modulus) % modulus))) % modulus + modulus) % modulus)
            x <- x + x
            y <- int (_floordiv (int y) (int 2))
        __ret <- ((res % modulus + modulus) % modulus)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (printfn "%s" (_str (binary_multiply (2) (3))))
        ignore (printfn "%s" (_str (binary_multiply (5) (0))))
        ignore (printfn "%s" (_str (binary_mod_multiply (2) (3) (5))))
        ignore (printfn "%s" (_str (binary_mod_multiply (10) (5) (13))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
