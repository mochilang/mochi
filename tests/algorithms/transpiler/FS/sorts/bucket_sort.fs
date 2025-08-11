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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec int_to_float (x: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (float x) * 1.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec floor_int (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable i: int = 0
        while (int_to_float (i + 1)) <= x do
            i <- i + 1
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
let rec set_at_float (xs: float array) (idx: int) (value: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable i: int = 0
        let mutable res: float array = Array.empty<float>
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec set_at_list_float (xs: float array array) (idx: int) (value: float array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable xs = xs
    let mutable idx = idx
    let mutable value = value
    try
        let mutable i: int = 0
        let mutable res: float array array = Array.empty<float array>
        while i < (Seq.length (xs)) do
            if i = idx then
                res <- Array.append res [|value|]
            else
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_float (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        let mutable res: float array = xs
        let mutable i: int = 1
        while i < (Seq.length (res)) do
            let key: float = _idx res (int i)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx res (int j)) > key) do
                res <- set_at_float (res) (j + 1) (_idx res (int j))
                j <- j - 1
            res <- set_at_float (res) (j + 1) (key)
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec bucket_sort_with_count (xs: float array) (bucket_count: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    let mutable bucket_count = bucket_count
    try
        if ((Seq.length (xs)) = 0) || (bucket_count <= 0) then
            __ret <- Array.empty<float>
            raise Return
        let mutable min_value: float = _idx xs (int 0)
        let mutable max_value: float = _idx xs (int 0)
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) < min_value then
                min_value <- _idx xs (int i)
            if (_idx xs (int i)) > max_value then
                max_value <- _idx xs (int i)
            i <- i + 1
        if max_value = min_value then
            __ret <- xs
            raise Return
        let bucket_size: float = (max_value - min_value) / (int_to_float (bucket_count))
        let mutable buckets: float array array = Array.empty<float array>
        i <- 0
        while i < bucket_count do
            buckets <- Array.append buckets [|[||]|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (xs)) do
            let ``val``: float = _idx xs (int i)
            let mutable idx: int = floor_int ((``val`` - min_value) / bucket_size)
            if idx < 0 then
                idx <- 0
            if idx >= bucket_count then
                idx <- bucket_count - 1
            let mutable bucket: float array = _idx buckets (int idx)
            bucket <- Array.append bucket [|``val``|]
            buckets <- set_at_list_float (buckets) (idx) (bucket)
            i <- i + 1
        let mutable result: float array = Array.empty<float>
        i <- 0
        while i < (Seq.length (buckets)) do
            let sorted_bucket: float array = sort_float (_idx buckets (int i))
            let mutable j: int = 0
            while j < (Seq.length (sorted_bucket)) do
                result <- Array.append result [|(_idx sorted_bucket (int j))|]
                j <- j + 1
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec bucket_sort (xs: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable xs = xs
    try
        __ret <- bucket_sort_with_count (xs) (10)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (bucket_sort (unbox<float array> [|-1.0; 2.0; -5.0; 0.0|])))
printfn "%s" (_str (bucket_sort (unbox<float array> [|9.0; 8.0; 7.0; 6.0; -12.0|])))
printfn "%s" (_str (bucket_sort (unbox<float array> [|0.4; 1.2; 0.1; 0.2; -0.9|])))
printfn "%s" (_str (bucket_sort (Array.empty<float>)))
printfn "%s" (_str (bucket_sort (unbox<float array> [|-10000000000.0; 10000000000.0|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
