// Generated 2025-08-26 08:36 +0700

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
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int64 = int64 0
        while i < (int64 20) do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + (int64 1)
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and squareplus (vector: float array) (beta: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vector = vector
    let mutable beta = beta
    try
        let mutable result: float array = Array.empty<float>
        let mutable i: int64 = int64 0
        while i < (int64 (Seq.length (vector))) do
            let x: float = _idx vector (int i)
            let ``val``: float = (x + (sqrtApprox ((x * x) + beta))) / 2.0
            result <- Array.append result [|``val``|]
            i <- i + (int64 1)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let v1: float array = unbox<float array> [|2.3; 0.6; -2.0; -3.8|]
        let v2: float array = unbox<float array> [|-9.2; -0.3; 0.45; -4.56|]
        ignore (printfn "%s" (_str (squareplus (v1) (2.0))))
        ignore (printfn "%s" (_str (squareplus (v2) (3.0))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
