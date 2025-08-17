// Generated 2025-08-17 08:49 +0700

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
    | :? float as f -> sprintf "%.15g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x >= 0.0 then
            __ret <- x
            raise Return
        else
            __ret <- -x
            raise Return
        __ret
    with
        | Return -> __ret
and chebyshev_distance (point_a: float array) (point_b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable point_a = point_a
    let mutable point_b = point_b
    try
        if (Seq.length (point_a)) <> (Seq.length (point_b)) then
            ignore (failwith ("Both points must have the same dimension."))
        let mutable max_diff: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (point_a)) do
            let diff: float = abs ((_idx point_a (int i)) - (_idx point_b (int i)))
            if diff > max_diff then
                max_diff <- diff
            i <- i + 1
        __ret <- max_diff
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (chebyshev_distance (unbox<float array> [|1.0; 1.0|]) (unbox<float array> [|2.0; 2.0|]))))
ignore (printfn "%s" (_str (chebyshev_distance (unbox<float array> [|1.0; 1.0; 9.0|]) (unbox<float array> [|2.0; 2.0; -5.2|]))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
