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
        let mutable i: int = start
        while i < ``end`` do
            result <- Array.append result [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and merge (left: int array) (right: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable left = left
    let mutable right = right
    try
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 0
        let mutable j: int = 0
        while (i < (Seq.length (left))) && (j < (Seq.length (right))) do
            if (_idx left (int i)) <= (_idx right (int j)) then
                result <- Array.append result [|(_idx left (int i))|]
                i <- i + 1
            else
                result <- Array.append result [|(_idx right (int j))|]
                j <- j + 1
        while i < (Seq.length (left)) do
            result <- Array.append result [|(_idx left (int i))|]
            i <- i + 1
        while j < (Seq.length (right)) do
            result <- Array.append result [|(_idx right (int j))|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and merge_sort (collection: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    try
        if (Seq.length (collection)) <= 1 then
            __ret <- collection
            raise Return
        let mid_index: int = _floordiv (Seq.length (collection)) 2
        let left: int array = subarray (collection) (0) (mid_index)
        let right: int array = subarray (collection) (mid_index) (Seq.length (collection))
        let sorted_left: int array = merge_sort (left)
        let sorted_right: int array = merge_sort (right)
        __ret <- merge (sorted_left) (sorted_right)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (merge_sort (unbox<int array> [|0; 5; 3; 2; 2|])))
printfn "%s" (_str (merge_sort (Array.empty<int>)))
printfn "%s" (_str (merge_sort (unbox<int array> [|-2; -5; -45|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
