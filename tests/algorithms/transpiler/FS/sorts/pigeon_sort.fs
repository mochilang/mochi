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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_list (n: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    let mutable value = value
    try
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 0
        while i < n do
            result <- Array.append result [|value|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and min_value (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let mutable m: int = _idx arr (int 0)
        let mutable i: int = 1
        while i < (Seq.length (arr)) do
            if (_idx arr (int i)) < m then
                m <- _idx arr (int i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and max_value (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let mutable m: int = _idx arr (int 0)
        let mutable i: int = 1
        while i < (Seq.length (arr)) do
            if (_idx arr (int i)) > m then
                m <- _idx arr (int i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and pigeon_sort (array: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable array = array
    try
        if (Seq.length (array)) = 0 then
            __ret <- array
            raise Return
        let mn: int = min_value (array)
        let mx: int = max_value (array)
        let holes_range: int = (mx - mn) + 1
        let mutable holes: int array = make_list (holes_range) (0)
        let mutable holes_repeat: int array = make_list (holes_range) (0)
        let mutable i: int = 0
        while i < (Seq.length (array)) do
            let index: int = (_idx array (int i)) - mn
            holes.[int index] <- _idx array (int i)
            holes_repeat.[int index] <- (_idx holes_repeat (int index)) + 1
            i <- i + 1
        let mutable array_index: int = 0
        let mutable h: int = 0
        while h < holes_range do
            while (_idx holes_repeat (int h)) > 0 do
                array.[int array_index] <- _idx holes (int h)
                array_index <- array_index + 1
                holes_repeat.[int h] <- (_idx holes_repeat (int h)) - 1
            h <- h + 1
        __ret <- array
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (pigeon_sort (unbox<int array> [|0; 5; 3; 2; 2|])))
printfn "%s" (_str (pigeon_sort (Array.empty<int>)))
printfn "%s" (_str (pigeon_sort (unbox<int array> [|-2; -5; -45|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
