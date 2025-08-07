// Generated 2025-08-07 14:57 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec parent_index (child_idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable child_idx = child_idx
    try
        __ret <- if child_idx > 0 then ((child_idx - 1) / 2) else (-1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec left_child_idx (parent_idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable parent_idx = parent_idx
    try
        __ret <- (2 * parent_idx) + 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec right_child_idx (parent_idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable parent_idx = parent_idx
    try
        __ret <- (2 * parent_idx) + 2
        raise Return
        __ret
    with
        | Return -> __ret
let rec max_heapify (h: float array) (heap_size: int) (index: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable heap_size = heap_size
    let mutable index = index
    try
        let mutable largest: int = index
        let left: int = left_child_idx (index)
        let right: int = right_child_idx (index)
        if (left < heap_size) && ((_idx h (left)) > (_idx h (largest))) then
            largest <- left
        if (right < heap_size) && ((_idx h (right)) > (_idx h (largest))) then
            largest <- right
        if largest <> index then
            let temp: float = _idx h (index)
            h.[index] <- _idx h (largest)
            h.[largest] <- temp
            max_heapify (h) (heap_size) (largest)
        __ret
    with
        | Return -> __ret
let rec build_max_heap (h: float array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    try
        let mutable heap_size: int = Seq.length (h)
        let mutable i: int = (heap_size / 2) - 1
        while i >= 0 do
            max_heapify (h) (heap_size) (i)
            i <- i - 1
        __ret <- heap_size
        raise Return
        __ret
    with
        | Return -> __ret
let rec extract_max (h: float array) (heap_size: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable h = h
    let mutable heap_size = heap_size
    try
        let max_value: float = _idx h (0)
        h.[0] <- _idx h (heap_size - 1)
        max_heapify (h) (heap_size - 1) (0)
        __ret <- max_value
        raise Return
        __ret
    with
        | Return -> __ret
let rec insert (h: float array) (heap_size: int) (value: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    let mutable heap_size = heap_size
    let mutable value = value
    try
        if heap_size < (Seq.length (h)) then
            h.[heap_size] <- value
        else
            h <- Array.append h [|value|]
        heap_size <- heap_size + 1
        let mutable idx: int = (heap_size - 1) / 2
        while idx >= 0 do
            max_heapify (h) (heap_size) (idx)
            idx <- (idx - 1) / 2
        __ret <- heap_size
        raise Return
        __ret
    with
        | Return -> __ret
let rec heap_sort (h: float array) (heap_size: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable heap_size = heap_size
    try
        let mutable size: int = heap_size
        let mutable j: int = size - 1
        while j > 0 do
            let temp: float = _idx h (0)
            h.[0] <- _idx h (j)
            h.[j] <- temp
            size <- size - 1
            max_heapify (h) (size) (0)
            j <- j - 1
        __ret
    with
        | Return -> __ret
let rec heap_to_string (h: float array) (heap_size: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable h = h
    let mutable heap_size = heap_size
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < heap_size do
            s <- s + (_str (_idx h (i)))
            if i < (heap_size - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let mutable heap: float array = [|103.0; 9.0; 1.0; 7.0; 11.0; 15.0; 25.0; 201.0; 209.0; 107.0; 5.0|]
let mutable size: int = build_max_heap (heap)
printfn "%s" (heap_to_string (heap) (size))
let m: float = extract_max (heap) (size)
size <- size - 1
printfn "%s" (_str (m))
printfn "%s" (heap_to_string (heap) (size))
size <- insert (heap) (size) (100.0)
printfn "%s" (heap_to_string (heap) (size))
heap_sort (heap) (size)
printfn "%s" (heap_to_string (heap) (size))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
