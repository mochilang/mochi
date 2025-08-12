// Generated 2025-08-12 07:47 +0700

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
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs_val (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and validate_point (p: float array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable p = p
    try
        if (Seq.length (p)) = 0 then
            failwith ("Missing an input")
        __ret
    with
        | Return -> __ret
and manhattan_distance (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        validate_point (a)
        validate_point (b)
        if (Seq.length (a)) <> (Seq.length (b)) then
            failwith ("Both points must be in the same n-dimensional space")
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            total <- total + (abs_val ((_idx a (int i)) - (_idx b (int i))))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and manhattan_distance_one_liner (a: float array) (b: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- manhattan_distance (a) (b)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (manhattan_distance (unbox<float array> [|1.0; 1.0|]) (unbox<float array> [|2.0; 2.0|])))
printfn "%s" (_str (manhattan_distance (unbox<float array> [|1.5; 1.5|]) (unbox<float array> [|2.0; 2.0|])))
printfn "%s" (_str (manhattan_distance_one_liner (unbox<float array> [|1.5; 1.5|]) (unbox<float array> [|2.5; 2.0|])))
printfn "%s" (_str (manhattan_distance_one_liner (unbox<float array> [|-3.0; -3.0; -3.0|]) (unbox<float array> [|0.0; 0.0; 0.0|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
