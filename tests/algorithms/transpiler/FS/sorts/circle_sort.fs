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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec circle_sort_util (collection: int array) (low: int) (high: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable collection = collection
    let mutable low = low
    let mutable high = high
    try
        let mutable swapped: bool = false
        if low = high then
            __ret <- swapped
            raise Return
        let mutable left: int = low
        let mutable right: int = high
        while left < right do
            if (_idx collection (int left)) > (_idx collection (int right)) then
                let tmp: int = _idx collection (int left)
                collection.[int left] <- _idx collection (int right)
                collection.[int right] <- tmp
                swapped <- true
            left <- left + 1
            right <- right - 1
        if (left = right) && ((_idx collection (int left)) > (_idx collection (int (right + 1)))) then
            let tmp2: int = _idx collection (int left)
            collection.[int left] <- _idx collection (int (right + 1))
            collection.[int (right + 1)] <- tmp2
            swapped <- true
        let mid: int = low + (_floordiv (high - low) 2)
        let left_swap: bool = circle_sort_util (collection) (low) (mid)
        let right_swap: bool = circle_sort_util (collection) (mid + 1) (high)
        if (swapped || left_swap) || right_swap then
            __ret <- true
            raise Return
        else
            __ret <- false
            raise Return
        __ret
    with
        | Return -> __ret
let rec circle_sort (collection: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    try
        if (Seq.length (collection)) < 2 then
            __ret <- collection
            raise Return
        let mutable is_not_sorted: bool = true
        while is_not_sorted do
            is_not_sorted <- circle_sort_util (collection) (0) ((Seq.length (collection)) - 1)
        __ret <- collection
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (circle_sort (unbox<int array> [|0; 5; 3; 2; 2|])))
printfn "%s" (_str (circle_sort (Array.empty<int>)))
printfn "%s" (_str (circle_sort (unbox<int array> [|-2; 5; 0; -45|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
