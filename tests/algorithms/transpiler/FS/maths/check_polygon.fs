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
let rec check_polygon (nums: float array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable nums = nums
    try
        if (Seq.length (nums)) < 2 then
            ignore (failwith ("Monogons and Digons are not polygons in the Euclidean space"))
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            if (_idx nums (int i)) <= 0.0 then
                ignore (failwith ("All values must be greater than 0"))
            i <- i + 1
        let mutable total: float = 0.0
        let mutable max_side: float = 0.0
        i <- 0
        while i < (Seq.length (nums)) do
            let v: float = _idx nums (int i)
            total <- total + v
            if v > max_side then
                max_side <- v
            i <- i + 1
        __ret <- max_side < (total - max_side)
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (check_polygon (unbox<float array> [|6.0; 10.0; 5.0|]))))
ignore (printfn "%s" (_str (check_polygon (unbox<float array> [|3.0; 7.0; 13.0; 2.0|]))))
ignore (printfn "%s" (_str (check_polygon (unbox<float array> [|1.0; 4.3; 5.2; 12.2|]))))
let mutable nums: float array = unbox<float array> [|3.0; 7.0; 13.0; 2.0|]
let _: bool = check_polygon (nums)
ignore (printfn "%s" (_str (nums)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
