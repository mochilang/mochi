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
let rec is_arithmetic_series (xs: float array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    try
        if (Seq.length (xs)) = 0 then
            ignore (failwith ("Input list must be a non empty list"))
        if (Seq.length (xs)) = 1 then
            __ret <- true
            raise Return
        let diff: float = (_idx xs (int 1)) - (_idx xs (int 0))
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            if ((_idx xs (int (i + 1))) - (_idx xs (int i))) <> diff then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and arithmetic_mean (xs: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable xs = xs
    try
        if (Seq.length (xs)) = 0 then
            ignore (failwith ("Input list must be a non empty list"))
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            total <- total + (_idx xs (int i))
            i <- i + 1
        __ret <- total / (float (Seq.length (xs)))
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (is_arithmetic_series (unbox<float array> [|2.0; 4.0; 6.0|]))))
ignore (printfn "%s" (_str (is_arithmetic_series (unbox<float array> [|3.0; 6.0; 12.0; 24.0|]))))
ignore (printfn "%s" (_str (arithmetic_mean (unbox<float array> [|2.0; 4.0; 6.0|]))))
ignore (printfn "%s" (_str (arithmetic_mean (unbox<float array> [|3.0; 6.0; 9.0; 12.0|]))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
