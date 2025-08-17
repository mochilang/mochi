// Generated 2025-08-17 12:28 +0700

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
let PI: float = 3.141592653589793
let rec radians (degree: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable degree = degree
    try
        __ret <- degree / (180.0 / PI)
        raise Return
        __ret
    with
        | Return -> __ret
and abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and almost_equal (a: float) (b: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        __ret <- (abs_float (a - b)) <= 0.00000001
        raise Return
        __ret
    with
        | Return -> __ret
and test_radians () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if not (almost_equal (radians (180.0)) (PI)) then
            ignore (failwith ("radians 180 failed"))
        if not (almost_equal (radians (92.0)) (1.6057029118347832)) then
            ignore (failwith ("radians 92 failed"))
        if not (almost_equal (radians (274.0)) (4.782202150464463)) then
            ignore (failwith ("radians 274 failed"))
        if not (almost_equal (radians (109.82)) (1.9167205845401725)) then
            ignore (failwith ("radians 109.82 failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_radians())
        ignore (printfn "%s" (_str (radians (180.0))))
        ignore (printfn "%s" (_str (radians (92.0))))
        ignore (printfn "%s" (_str (radians (274.0))))
        ignore (printfn "%s" (_str (radians (109.82))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
