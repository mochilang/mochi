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
let rec subarray (xs: int array) (start: int) (``end``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable result: int array = [||]
        let mutable k: int = start
        while k < ``end`` do
            result <- Array.append result [|_idx xs (k)|]
            k <- k + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec merge (left_half: int array) (right_half: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable left_half = left_half
    let mutable right_half = right_half
    try
        let mutable result: int array = [||]
        let mutable i: int = 0
        let mutable j: int = 0
        while (i < (Seq.length (left_half))) && (j < (Seq.length (right_half))) do
            if (_idx left_half (i)) < (_idx right_half (j)) then
                result <- Array.append result [|_idx left_half (i)|]
                i <- i + 1
            else
                result <- Array.append result [|_idx right_half (j)|]
                j <- j + 1
        while i < (Seq.length (left_half)) do
            result <- Array.append result [|_idx left_half (i)|]
            i <- i + 1
        while j < (Seq.length (right_half)) do
            result <- Array.append result [|_idx right_half (j)|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec merge_sort (array: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable array = array
    try
        if (Seq.length (array)) <= 1 then
            __ret <- array
            raise Return
        let middle: int = (Seq.length (array)) / 2
        let left_half: int array = subarray (array) (0) (middle)
        let right_half: int array = subarray (array) (middle) (Seq.length (array))
        let sorted_left: int array = merge_sort (left_half)
        let sorted_right: int array = merge_sort (right_half)
        __ret <- merge (sorted_left) (sorted_right)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (merge_sort (unbox<int array> [|5; 3; 1; 4; 2|])))
printfn "%s" (_str (merge_sort (unbox<int array> [|-2; 3; -10; 11; 99; 100000; 100; -200|])))
printfn "%s" (_str (merge_sort (unbox<int array> [|-200|])))
printfn "%s" (_str (merge_sort (Array.empty<int>)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
