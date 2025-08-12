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
let rec max_sum_sliding_window (arr: int array) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable k = k
    try
        if (k < 0) || ((Seq.length (arr)) < k) then
            failwith ("Invalid Input")
        let mutable idx: int = 0
        let mutable current_sum: int = 0
        while idx < k do
            current_sum <- current_sum + (_idx arr (int idx))
            idx <- idx + 1
        let mutable max_sum: int = current_sum
        let mutable i: int = 0
        while i < ((Seq.length (arr)) - k) do
            current_sum <- (current_sum - (_idx arr (int i))) + (_idx arr (int (i + k)))
            if current_sum > max_sum then
                max_sum <- current_sum
            i <- i + 1
        __ret <- max_sum
        raise Return
        __ret
    with
        | Return -> __ret
and test_max_sum_sliding_window () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let arr1: int array = unbox<int array> [|1; 4; 2; 10; 2; 3; 1; 0; 20|]
        if (max_sum_sliding_window (arr1) (4)) <> 24 then
            failwith ("test1 failed")
        let arr2: int array = unbox<int array> [|1; 4; 2; 10; 2; 13; 1; 0; 2|]
        if (max_sum_sliding_window (arr2) (4)) <> 27 then
            failwith ("test2 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_max_sum_sliding_window()
        let sample: int array = unbox<int array> [|1; 4; 2; 10; 2; 3; 1; 0; 20|]
        printfn "%s" (_str (max_sum_sliding_window (sample) (4)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
