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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_sorted (xs: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    try
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            if (_idx xs (int (i - 1))) > (_idx xs (int i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec exponential_search (arr: int array) (item: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable item = item
    try
        if not (is_sorted (arr)) then
            failwith ("sorted_collection must be sorted in ascending order")
        if (Seq.length (arr)) = 0 then
            __ret <- -1
            raise Return
        if (_idx arr (int 0)) = item then
            __ret <- 0
            raise Return
        let mutable bound: int = 1
        while (bound < (Seq.length (arr))) && ((_idx arr (int bound)) < item) do
            bound <- int ((int64 bound) * (int64 2))
        let mutable left: int = _floordiv bound 2
        let mutable right: int = bound
        if right >= (Seq.length (arr)) then
            right <- (Seq.length (arr)) - 1
        while left <= right do
            let mid: int = left + (_floordiv (right - left) 2)
            if (_idx arr (int mid)) = item then
                __ret <- mid
                raise Return
            if (_idx arr (int mid)) > item then
                right <- mid - 1
            else
                left <- mid + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
