// Generated 2025-08-11 16:20 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec heapify (arr: int array) (index: int) (heap_size: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable arr = arr
    let mutable index = index
    let mutable heap_size = heap_size
    try
        let mutable largest: int = index
        let left_index: int64 = ((int64 2) * (int64 index)) + (int64 1)
        let right_index: int64 = ((int64 2) * (int64 index)) + (int64 2)
        if (left_index < (int64 heap_size)) && ((_idx arr (int left_index)) > (_idx arr (int largest))) then
            largest <- int left_index
        if (right_index < (int64 heap_size)) && ((_idx arr (int right_index)) > (_idx arr (int largest))) then
            largest <- int right_index
        if largest <> index then
            let temp: int = _idx arr (int largest)
            arr.[int largest] <- _idx arr (int index)
            arr.[int index] <- temp
            heapify (arr) (largest) (heap_size)
        __ret
    with
        | Return -> __ret
let rec heap_sort (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        let n: int = Seq.length (arr)
        let mutable i: int = (_floordiv n 2) - 1
        while i >= 0 do
            heapify (arr) (i) (n)
            i <- i - 1
        i <- n - 1
        while i > 0 do
            let temp: int = _idx arr (int 0)
            arr.[int 0] <- _idx arr (int i)
            arr.[int i] <- temp
            heapify (arr) (0) (i)
            i <- i - 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let mutable data: int array = unbox<int array> [|3; 7; 9; 28; 123; -5; 8; -30; -200; 0; 4|]
let mutable result: int array = heap_sort (data)
printfn "%s" (_repr (result))
if (_str (result)) <> (_str ([|-200; -30; -5; 0; 3; 4; 7; 8; 9; 28; 123|])) then
    failwith ("Assertion error")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
