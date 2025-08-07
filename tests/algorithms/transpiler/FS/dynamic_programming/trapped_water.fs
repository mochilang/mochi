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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_list (len: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable len = len
    let mutable value = value
    try
        let mutable arr: int array = [||]
        let mutable i: int = 0
        while i < len do
            arr <- Array.append arr [|value|]
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec trapped_rainwater (heights: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable heights = heights
    try
        if (Seq.length (heights)) = 0 then
            __ret <- 0
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (heights)) do
            if (_idx heights (i)) < 0 then
                failwith ("No height can be negative")
            i <- i + 1
        let length: int = Seq.length (heights)
        let mutable left_max: int array = make_list (length) (0)
        left_max.[0] <- _idx heights (0)
        i <- 1
        while i < length do
            if (_idx heights (i)) > (_idx left_max (i - 1)) then
                left_max.[i] <- _idx heights (i)
            else
                left_max.[i] <- _idx left_max (i - 1)
            i <- i + 1
        let mutable right_max: int array = make_list (length) (0)
        let last: int = length - 1
        right_max.[last] <- _idx heights (last)
        i <- last - 1
        while i >= 0 do
            if (_idx heights (i)) > (_idx right_max (i + 1)) then
                right_max.[i] <- _idx heights (i)
            else
                right_max.[i] <- _idx right_max (i + 1)
            i <- i - 1
        let mutable total: int = 0
        i <- 0
        while i < length do
            let left: int = _idx left_max (i)
            let right: int = _idx right_max (i)
            let smaller: int = if left < right then left else right
            total <- total + (smaller - (_idx heights (i)))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (trapped_rainwater (unbox<int array> [|0; 1; 0; 2; 1; 0; 1; 3; 2; 1; 2; 1|])))
printfn "%s" (_str (trapped_rainwater (unbox<int array> [|7; 1; 5; 3; 6; 4|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
