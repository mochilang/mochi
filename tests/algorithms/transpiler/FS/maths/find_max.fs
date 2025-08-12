// Generated 2025-08-12 07:47 +0700

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
let rec normalize_index (index: int) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable index = index
    let mutable n = n
    try
        __ret <- if index < 0 then (n + index) else index
        raise Return
        __ret
    with
        | Return -> __ret
and find_max_iterative (nums: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            failwith ("find_max_iterative() arg is an empty sequence")
        let mutable max_num: float = _idx nums (int 0)
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            let x: float = _idx nums (int i)
            if x > max_num then
                max_num <- x
            i <- i + 1
        __ret <- max_num
        raise Return
        __ret
    with
        | Return -> __ret
and find_max_recursive (nums: float array) (left: int) (right: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    let mutable left = left
    let mutable right = right
    try
        let n: int = Seq.length (nums)
        if n = 0 then
            failwith ("find_max_recursive() arg is an empty sequence")
        if (((left >= n) || (left < (0 - n))) || (right >= n)) || (right < (0 - n)) then
            failwith ("list index out of range")
        let mutable l: int = normalize_index (left) (n)
        let mutable r: int = normalize_index (right) (n)
        if l = r then
            __ret <- _idx nums (int l)
            raise Return
        let mid: int = _floordiv (l + r) 2
        let left_max: float = find_max_recursive (nums) (l) (mid)
        let right_max: float = find_max_recursive (nums) (mid + 1) (r)
        if left_max >= right_max then
            __ret <- left_max
            raise Return
        __ret <- right_max
        raise Return
        __ret
    with
        | Return -> __ret
and test_find_max () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let arr: float array = unbox<float array> [|2.0; 4.0; 9.0; 7.0; 19.0; 94.0; 5.0|]
        if (find_max_iterative (arr)) <> 94.0 then
            failwith ("find_max_iterative failed")
        if (find_max_recursive (arr) (0) ((Seq.length (arr)) - 1)) <> 94.0 then
            failwith ("find_max_recursive failed")
        if (find_max_recursive (arr) (-(Seq.length (arr))) (-1)) <> 94.0 then
            failwith ("negative index handling failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_find_max()
        let nums: float array = unbox<float array> [|2.0; 4.0; 9.0; 7.0; 19.0; 94.0; 5.0|]
        printfn "%g" (find_max_iterative (nums))
        printfn "%g" (find_max_recursive (nums) (0) ((Seq.length (nums)) - 1))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
