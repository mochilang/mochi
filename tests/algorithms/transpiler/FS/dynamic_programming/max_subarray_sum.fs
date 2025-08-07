// Generated 2025-08-07 15:46 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec max_subarray_sum (nums: float array) (allow_empty: bool) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    let mutable allow_empty = allow_empty
    try
        if (Seq.length (nums)) = 0 then
            __ret <- 0.0
            raise Return
        let mutable max_sum: float = 0.0
        let mutable curr_sum: float = 0.0
        if allow_empty then
            max_sum <- 0.0
            curr_sum <- 0.0
            let mutable i: int = 0
            while i < (Seq.length (nums)) do
                let num: float = _idx nums (i)
                let temp: float = curr_sum + num
                curr_sum <- if temp > 0.0 then temp else 0.0
                if curr_sum > max_sum then
                    max_sum <- curr_sum
                i <- i + 1
        else
            max_sum <- _idx nums (0)
            curr_sum <- _idx nums (0)
            let mutable i: int = 1
            while i < (Seq.length (nums)) do
                let num: float = _idx nums (i)
                let temp: float = curr_sum + num
                curr_sum <- if temp > num then temp else num
                if curr_sum > max_sum then
                    max_sum <- curr_sum
                i <- i + 1
        __ret <- max_sum
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (max_subarray_sum (unbox<float array> [|2.0; 8.0; 9.0|]) (false)))
printfn "%s" (_str (max_subarray_sum (unbox<float array> [|0.0; 0.0|]) (false)))
printfn "%s" (_str (max_subarray_sum (unbox<float array> [|-1.0; 0.0; 1.0|]) (false)))
printfn "%s" (_str (max_subarray_sum (unbox<float array> [|1.0; 2.0; 3.0; 4.0; -2.0|]) (false)))
printfn "%s" (_str (max_subarray_sum (unbox<float array> [|-2.0; 1.0; -3.0; 4.0; -1.0; 2.0; 1.0; -5.0; 4.0|]) (false)))
printfn "%s" (_str (max_subarray_sum (unbox<float array> [|2.0; 3.0; -9.0; 8.0; -2.0|]) (false)))
printfn "%s" (_str (max_subarray_sum (unbox<float array> [|-2.0; -3.0; -1.0; -4.0; -6.0|]) (false)))
printfn "%s" (_str (max_subarray_sum (unbox<float array> [|-2.0; -3.0; -1.0; -4.0; -6.0|]) (true)))
let mutable empty: float array = [||]
printfn "%s" (_str (max_subarray_sum (empty) (false)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
