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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
let rec is_harmonic_series (series: float array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable series = series
    try
        if (Seq.length (series)) = 0 then
            ignore (failwith ("Input list must be a non empty list"))
        if (Seq.length (series)) = 1 then
            if (_idx series (int 0)) = 0.0 then
                ignore (failwith ("Input series cannot have 0 as an element"))
            __ret <- true
            raise Return
        let mutable rec_series: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (series)) do
            let ``val``: float = _idx series (int i)
            if ``val`` = 0.0 then
                ignore (failwith ("Input series cannot have 0 as an element"))
            rec_series <- Array.append rec_series [|(1.0 / ``val``)|]
            i <- i + 1
        let common_diff: float = (_idx rec_series (int 1)) - (_idx rec_series (int 0))
        let mutable idx: int = 2
        while idx < (Seq.length (rec_series)) do
            if ((_idx rec_series (int idx)) - (_idx rec_series (int (idx - 1)))) <> common_diff then
                __ret <- false
                raise Return
            idx <- idx + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and harmonic_mean (series: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable series = series
    try
        if (Seq.length (series)) = 0 then
            ignore (failwith ("Input list must be a non empty list"))
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (series)) do
            total <- total + (1.0 / (_idx series (int i)))
            i <- i + 1
        __ret <- (float (Seq.length (series))) / total
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%b" (is_harmonic_series (unbox<float array> [|1.0; 2.0 / 3.0; 1.0 / 2.0; 2.0 / 5.0; 1.0 / 3.0|])))
ignore (printfn "%b" (is_harmonic_series (unbox<float array> [|1.0; 2.0 / 3.0; 2.0 / 5.0; 1.0 / 3.0|])))
ignore (printfn "%s" (_str (harmonic_mean (unbox<float array> [|1.0; 4.0; 4.0|]))))
ignore (printfn "%s" (_str (harmonic_mean (unbox<float array> [|3.0; 6.0; 9.0; 12.0|]))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
