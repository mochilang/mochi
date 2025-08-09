// Generated 2025-08-09 10:14 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
        let mutable k: int = start
        while k < ``end`` do
            result <- Array.append result [|(_idx xs (int k))|]
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
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 0
        let mutable j: int = 0
        while (i < (Seq.length (left_half))) && (j < (Seq.length (right_half))) do
            if (_idx left_half (int i)) < (_idx right_half (int j)) then
                result <- Array.append result [|(_idx left_half (int i))|]
                i <- i + 1
            else
                result <- Array.append result [|(_idx right_half (int j))|]
                j <- j + 1
        while i < (Seq.length (left_half)) do
            result <- Array.append result [|(_idx left_half (int i))|]
            i <- i + 1
        while j < (Seq.length (right_half)) do
            result <- Array.append result [|(_idx right_half (int j))|]
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
        let middle: int = _floordiv (Seq.length (array)) 2
        let left_half: int array = subarray (array) (0) (middle)
        let right_half: int array = subarray (array) (middle) (Seq.length (array))
        let sorted_left: int array = merge_sort (left_half)
        let sorted_right: int array = merge_sort (right_half)
        __ret <- merge (sorted_left) (sorted_right)
        raise Return
        __ret
    with
        | Return -> __ret
let rec h_index (citations: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable citations = citations
    try
        let mutable idx: int = 0
        while idx < (Seq.length (citations)) do
            if (_idx citations (int idx)) < 0 then
                failwith ("The citations should be a list of non negative integers.")
            idx <- idx + 1
        let sorted: int array = merge_sort (citations)
        let n: int = Seq.length (sorted)
        let mutable i: int = 0
        while i < n do
            if (_idx sorted (int ((n - 1) - i))) <= i then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (h_index (unbox<int array> [|3; 0; 6; 1; 5|])))
printfn "%s" (_str (h_index (unbox<int array> [|1; 3; 1|])))
printfn "%s" (_str (h_index (unbox<int array> [|1; 2; 3|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
