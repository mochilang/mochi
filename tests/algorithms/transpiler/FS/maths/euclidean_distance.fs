// Generated 2025-08-12 08:17 +0700

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
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and euclidean_distance (v1: float array) (v2: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable v1 = v1
    let mutable v2 = v2
    try
        let mutable sum: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (v1)) do
            let diff: float = (_idx v1 (int i)) - (_idx v2 (int i))
            sum <- sum + (diff * diff)
            i <- i + 1
        __ret <- sqrtApprox (sum)
        raise Return
        __ret
    with
        | Return -> __ret
and euclidean_distance_no_np (v1: float array) (v2: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable v1 = v1
    let mutable v2 = v2
    try
        __ret <- euclidean_distance (v1) (v2)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (euclidean_distance (unbox<float array> [|0.0; 0.0|]) (unbox<float array> [|2.0; 2.0|])))
        printfn "%s" (_str (euclidean_distance (unbox<float array> [|0.0; 0.0; 0.0|]) (unbox<float array> [|2.0; 2.0; 2.0|])))
        printfn "%s" (_str (euclidean_distance (unbox<float array> [|1.0; 2.0; 3.0; 4.0|]) (unbox<float array> [|5.0; 6.0; 7.0; 8.0|])))
        printfn "%s" (_str (euclidean_distance_no_np (unbox<float array> [|1.0; 2.0; 3.0; 4.0|]) (unbox<float array> [|5.0; 6.0; 7.0; 8.0|])))
        printfn "%s" (_str (euclidean_distance_no_np (unbox<float array> [|0.0; 0.0|]) (unbox<float array> [|2.0; 2.0|])))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
