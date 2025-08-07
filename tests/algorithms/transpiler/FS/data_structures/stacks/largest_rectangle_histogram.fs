// Generated 2025-08-07 14:57 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec largest_rectangle_area (heights: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable heights = heights
    try
        let mutable stack: int array = [||]
        let mutable max_area: int = 0
        let mutable hs: int array = heights
        hs <- Array.append hs [|0|]
        let mutable i: int = 0
        while i < (Seq.length (hs)) do
            while ((Seq.length (stack)) > 0) && ((_idx hs (i)) < (_idx hs (_idx stack ((Seq.length (stack)) - 1)))) do
                let top: int = _idx stack ((Seq.length (stack)) - 1)
                stack <- Array.sub stack 0 (((Seq.length (stack)) - 1) - 0)
                let height: int = _idx hs (top)
                let mutable width: int = i
                if (Seq.length (stack)) > 0 then
                    width <- (i - (_idx stack ((Seq.length (stack)) - 1))) - 1
                let area: int = height * width
                if area > max_area then
                    max_area <- area
            stack <- Array.append stack [|i|]
            i <- i + 1
        __ret <- max_area
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (largest_rectangle_area (unbox<int array> [|2; 1; 5; 6; 2; 3|])))
printfn "%s" (_str (largest_rectangle_area (unbox<int array> [|2; 4|])))
printfn "%s" (_str (largest_rectangle_area (unbox<int array> [|6; 2; 5; 4; 5; 1; 6|])))
printfn "%s" (_str (largest_rectangle_area (unbox<int array> [|1|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
