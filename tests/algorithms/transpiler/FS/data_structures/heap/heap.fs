// Generated 2025-08-24 23:57 +0700

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
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec parent_index (child_idx: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable child_idx = child_idx
    try
        __ret <- if child_idx > (int64 0) then (_floordiv64 (int64 (child_idx - (int64 1))) (int64 (int64 2))) else (int64 (-1))
        raise Return
        __ret
    with
        | Return -> __ret
and left_child_idx (parent_idx: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable parent_idx = parent_idx
    try
        __ret <- ((int64 2) * parent_idx) + (int64 1)
        raise Return
        __ret
    with
        | Return -> __ret
and right_child_idx (parent_idx: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable parent_idx = parent_idx
    try
        __ret <- ((int64 2) * parent_idx) + (int64 2)
        raise Return
        __ret
    with
        | Return -> __ret
and max_heapify (h: float array) (heap_size: int64) (index: int64) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable h = h
    let mutable heap_size = heap_size
    let mutable index = index
    try
        let mutable largest: int64 = index
        let left: int64 = left_child_idx (index)
        let right: int64 = right_child_idx (index)
        if (left < heap_size) && ((_idx h (int left)) > (_idx h (int largest))) then
            largest <- left
        if (right < heap_size) && ((_idx h (int right)) > (_idx h (int largest))) then
            largest <- right
        if largest <> index then
            let temp: float = _idx h (int index)
            h.[int index] <- _idx h (int largest)
            h.[int largest] <- temp
            ignore (max_heapify (h) (heap_size) (largest))
        __ret
    with
        | Return -> __ret
and build_max_heap (h: float array) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable h = h
    try
        let mutable heap_size: int64 = int64 (Seq.length (h))
        let mutable i: int64 = (_floordiv64 (int64 heap_size) (int64 (int64 2))) - (int64 1)
        while i >= (int64 0) do
            ignore (max_heapify (h) (heap_size) (i))
            i <- i - (int64 1)
        __ret <- heap_size
        raise Return
        __ret
    with
        | Return -> __ret
and extract_max (h: float array) (heap_size: int64) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable h = h
    let mutable heap_size = heap_size
    try
        let max_value: float = _idx h (int 0)
        h.[0] <- _idx h (int (heap_size - (int64 1)))
        ignore (max_heapify (h) (heap_size - (int64 1)) (int64 0))
        __ret <- max_value
        raise Return
        __ret
    with
        | Return -> __ret
and insert (h: float array) (heap_size: int64) (value: float) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable h = h
    let mutable heap_size = heap_size
    let mutable value = value
    try
        if heap_size < (int64 (Seq.length (h))) then
            h.[int heap_size] <- value
        else
            h <- Array.append h [|value|]
        heap_size <- heap_size + (int64 1)
        let mutable idx: int64 = _floordiv64 (int64 (heap_size - (int64 1))) (int64 (int64 2))
        while idx >= (int64 0) do
            ignore (max_heapify (h) (heap_size) (idx))
            idx <- _floordiv64 (int64 (idx - (int64 1))) (int64 (int64 2))
        __ret <- heap_size
        raise Return
        __ret
    with
        | Return -> __ret
and heap_sort (h: float array) (heap_size: int64) =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    let mutable h = h
    let mutable heap_size = heap_size
    try
        let mutable size: int64 = heap_size
        let mutable j: int64 = size - (int64 1)
        while j > (int64 0) do
            let temp: float = _idx h (int 0)
            h.[0] <- _idx h (int j)
            h.[int j] <- temp
            size <- size - (int64 1)
            ignore (max_heapify (h) (size) (int64 0))
            j <- j - (int64 1)
        __ret
    with
        | Return -> __ret
and heap_to_string (h: float array) (heap_size: int64) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable h = h
    let mutable heap_size = heap_size
    try
        let mutable s: string = "["
        let mutable i: int64 = int64 0
        while i < heap_size do
            s <- s + (_str (_idx h (int i)))
            if i < (heap_size - (int64 1)) then
                s <- s + ", "
            i <- i + (int64 1)
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let mutable heap: float array = unbox<float array> [|103.0; 9.0; 1.0; 7.0; 11.0; 15.0; 25.0; 201.0; 209.0; 107.0; 5.0|]
let mutable size: int64 = build_max_heap (heap)
ignore (printfn "%s" (heap_to_string (heap) (size)))
let m: float = extract_max (heap) (size)
size <- size - (int64 1)
ignore (printfn "%s" (_str (m)))
ignore (printfn "%s" (heap_to_string (heap) (size)))
size <- insert (heap) (size) (100.0)
ignore (printfn "%s" (heap_to_string (heap) (size)))
ignore (heap_sort (heap) (size))
ignore (printfn "%s" (heap_to_string (heap) (size)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
