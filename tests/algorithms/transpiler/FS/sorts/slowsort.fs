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
let rec swap (seq: int array) (i: int) (j: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable seq = seq
    let mutable i = i
    let mutable j = j
    try
        let temp: int = _idx seq (int i)
        seq.[int i] <- _idx seq (int j)
        seq.[int j] <- temp
        __ret
    with
        | Return -> __ret
and slowsort_recursive (seq: int array) (start: int) (end_index: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable seq = seq
    let mutable start = start
    let mutable end_index = end_index
    try
        if start >= end_index then
            __ret <- ()
            raise Return
        let mid: int = _floordiv (start + end_index) 2
        slowsort_recursive (seq) (start) (mid)
        slowsort_recursive (seq) (mid + 1) (end_index)
        if (_idx seq (int end_index)) < (_idx seq (int mid)) then
            swap (seq) (end_index) (mid)
        slowsort_recursive (seq) (start) (end_index - 1)
        __ret
    with
        | Return -> __ret
and slow_sort (seq: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable seq = seq
    try
        if (Seq.length (seq)) > 0 then
            slowsort_recursive (seq) (0) ((Seq.length (seq)) - 1)
        __ret <- seq
        raise Return
        __ret
    with
        | Return -> __ret
let seq1: int array = unbox<int array> [|1; 6; 2; 5; 3; 4; 4; 5|]
printfn "%s" (_str (slow_sort (seq1)))
let mutable seq2: int array = Array.empty<int>
printfn "%s" (_str (slow_sort (seq2)))
let seq3: int array = unbox<int array> [|2|]
printfn "%s" (_str (slow_sort (seq3)))
let seq4: int array = unbox<int array> [|1; 2; 3; 4|]
printfn "%s" (_str (slow_sort (seq4)))
let seq5: int array = unbox<int array> [|4; 3; 2; 1|]
printfn "%s" (_str (slow_sort (seq5)))
let seq6: int array = unbox<int array> [|9; 8; 7; 6; 5; 4; 3; 2; 1; 0|]
slowsort_recursive (seq6) (2) (7)
printfn "%s" (_str (seq6))
let seq7: int array = unbox<int array> [|9; 8; 7; 6; 5; 4; 3; 2; 1; 0|]
slowsort_recursive (seq7) (0) (4)
printfn "%s" (_str (seq7))
let seq8: int array = unbox<int array> [|9; 8; 7; 6; 5; 4; 3; 2; 1; 0|]
slowsort_recursive (seq8) (5) ((Seq.length (seq8)) - 1)
printfn "%s" (_str (seq8))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
