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
let rec min_slice (a: int array) (start: int) (``end``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable m: int = _idx a (start)
        let mutable i: int = start + 1
        while i < ``end`` do
            if (_idx a (i)) < m then
                m <- _idx a (i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and max_slice (a: int array) (start: int) (``end``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable m: int = _idx a (start)
        let mutable i: int = start + 1
        while i < ``end`` do
            if (_idx a (i)) > m then
                m <- _idx a (i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and max_diff_range (a: int array) (start: int) (``end``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        if (``end`` - start) = 1 then
            let v: int = _idx a (start)
            __ret <- unbox<int array> [|v; v|]
            raise Return
        let mid: int = (start + ``end``) / 2
        let left: int array = max_diff_range (a) (start) (mid)
        let right: int array = max_diff_range (a) (mid) (``end``)
        let small1: int = _idx left (0)
        let big1: int = _idx left (1)
        let small2: int = _idx right (0)
        let big2: int = _idx right (1)
        let min_left: int = min_slice (a) (start) (mid)
        let max_right: int = max_slice (a) (mid) (``end``)
        let cross_diff: int = max_right - min_left
        let left_diff: int = big1 - small1
        let right_diff: int = big2 - small2
        if (right_diff > cross_diff) && (right_diff > left_diff) then
            __ret <- unbox<int array> [|small2; big2|]
            raise Return
        else
            if left_diff > cross_diff then
                __ret <- unbox<int array> [|small1; big1|]
                raise Return
            else
                __ret <- unbox<int array> [|min_left; max_right|]
                raise Return
        __ret
    with
        | Return -> __ret
and max_difference (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        __ret <- max_diff_range (a) (0) (Seq.length (a))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let result: int array = max_difference (unbox<int array> [|5; 11; 2; 1; 7; 9; 0; 7|])
        printfn "%s" (_str (result))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
