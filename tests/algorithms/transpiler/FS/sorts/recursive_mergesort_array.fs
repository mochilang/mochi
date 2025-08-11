// Generated 2025-08-11 17:23 +0700

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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec subarray (xs: int array) (start: int) (``end``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable result: int array = Array.empty<int>
        let mutable k: int = start
        while k < ``end`` do
            result <- Array.append result [|(_idx xs (int k))|]
            k <- k + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and merge (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        if (Seq.length (arr)) > 1 then
            let middle_length: int = _floordiv (Seq.length (arr)) 2
            let left_array: int array = subarray (arr) (0) (middle_length)
            let right_array: int array = subarray (arr) (middle_length) (Seq.length (arr))
            let left_size: int = Seq.length (left_array)
            let right_size: int = Seq.length (right_array)
            ignore (merge (left_array))
            ignore (merge (right_array))
            let mutable left_index: int = 0
            let mutable right_index: int = 0
            let mutable index: int = 0
            while (left_index < left_size) && (right_index < right_size) do
                if (_idx left_array (int left_index)) < (_idx right_array (int right_index)) then
                    arr.[int index] <- _idx left_array (int left_index)
                    left_index <- left_index + 1
                else
                    arr.[int index] <- _idx right_array (int right_index)
                    right_index <- right_index + 1
                index <- index + 1
            while left_index < left_size do
                arr.[int index] <- _idx left_array (int left_index)
                left_index <- left_index + 1
                index <- index + 1
            while right_index < right_size do
                arr.[int index] <- _idx right_array (int right_index)
                right_index <- right_index + 1
                index <- index + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (merge (unbox<int array> [|10; 9; 8; 7; 6; 5; 4; 3; 2; 1|])))
printfn "%s" (_str (merge (unbox<int array> [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|])))
printfn "%s" (_str (merge (unbox<int array> [|10; 22; 1; 2; 3; 9; 15; 23|])))
printfn "%s" (_str (merge (unbox<int array> [|100|])))
printfn "%s" (_str (merge (Array.empty<int>)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
