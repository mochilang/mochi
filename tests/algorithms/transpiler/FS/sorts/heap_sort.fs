// Generated 2025-08-24 08:57 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
     .Replace("L", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec heapify (arr: int64 array) (index: int64) (heap_size: int64) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable arr = arr
    let mutable index = index
    let mutable heap_size = heap_size
    try
        let mutable largest: int64 = index
        let left_index: int64 = ((int64 2) * index) + (int64 1)
        let right_index: int64 = ((int64 2) * index) + (int64 2)
        if (left_index < heap_size) && ((_idx arr (int left_index)) > (_idx arr (int largest))) then
            largest <- left_index
        if (right_index < heap_size) && ((_idx arr (int right_index)) > (_idx arr (int largest))) then
            largest <- right_index
        if largest <> index then
            let temp: int64 = _idx arr (int largest)
            arr.[int largest] <- _idx arr (int index)
            arr.[int index] <- temp
            ignore (heapify (arr) (largest) (heap_size))
        __ret
    with
        | Return -> __ret
and heap_sort (arr: int64 array) =
    let mutable __ret : int64 array = Unchecked.defaultof<int64 array>
    let mutable arr = arr
    try
        let n: int64 = int64 (Seq.length (arr))
        let mutable i: int64 = (_floordiv64 (int64 n) (int64 (int64 2))) - (int64 1)
        while i >= (int64 0) do
            ignore (heapify (arr) (i) (n))
            i <- i - (int64 1)
        i <- n - (int64 1)
        while i > (int64 0) do
            let temp: int64 = _idx arr (int 0)
            arr.[0] <- _idx arr (int i)
            arr.[int i] <- temp
            ignore (heapify (arr) (int64 0) (i))
            i <- i - (int64 1)
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let mutable data: int64 array = Array.map int64 [|3; 7; 9; 28; 123; -5; 8; -30; -200; 0; 4|]
let mutable result: int64 array = heap_sort (data)
ignore (printfn "%s" (_repr (result)))
if (_str (result)) <> (_str ([|-200; -30; -5; 0; 3; 4; 7; 8; 9; 28; 123|])) then
    ignore (failwith ("Assertion error"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
