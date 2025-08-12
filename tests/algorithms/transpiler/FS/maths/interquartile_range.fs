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
let rec bubble_sort (nums: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable nums = nums
    try
        let mutable arr: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (nums)) do
            arr <- Array.append arr [|(_idx nums (int i))|]
            i <- i + 1
        let mutable n: int = Seq.length (arr)
        let mutable a: int = 0
        while a < n do
            let mutable b: int = 0
            while b < ((n - a) - 1) do
                if (_idx arr (int b)) > (_idx arr (int (b + 1))) then
                    let temp: float = _idx arr (int b)
                    arr.[int b] <- _idx arr (int (b + 1))
                    arr.[int (b + 1)] <- temp
                b <- b + 1
            a <- a + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and find_median (nums: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    try
        let length: int = Seq.length (nums)
        let div: int = _floordiv length 2
        let ``mod``: int = ((length % 2 + 2) % 2)
        if ``mod`` <> 0 then
            __ret <- _idx nums (int div)
            raise Return
        __ret <- ((_idx nums (int div)) + (_idx nums (int (div - 1)))) / 2.0
        raise Return
        __ret
    with
        | Return -> __ret
and interquartile_range (nums: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            failwith ("The list is empty. Provide a non-empty list.")
        let sorted: float array = bubble_sort (nums)
        let length: int = Seq.length (sorted)
        let div: int = _floordiv length 2
        let ``mod``: int = ((length % 2 + 2) % 2)
        let mutable lower: float array = Array.empty<float>
        let mutable i: int = 0
        while i < div do
            lower <- Array.append lower [|(_idx sorted (int i))|]
            i <- i + 1
        let mutable upper: float array = Array.empty<float>
        let mutable j: int = div + ``mod``
        while j < length do
            upper <- Array.append upper [|(_idx sorted (int j))|]
            j <- j + 1
        let q1: float = find_median (lower)
        let q3: float = find_median (upper)
        __ret <- q3 - q1
        raise Return
        __ret
    with
        | Return -> __ret
and absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and float_equal (a: float) (b: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        let diff: float = absf (a - b)
        __ret <- diff < 0.0000001
        raise Return
        __ret
    with
        | Return -> __ret
and test_interquartile_range () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if not (float_equal (interquartile_range (unbox<float array> [|4.0; 1.0; 2.0; 3.0; 2.0|])) (2.0)) then
            failwith ("interquartile_range case1 failed")
        if not (float_equal (interquartile_range (unbox<float array> [|-2.0; -7.0; -10.0; 9.0; 8.0; 4.0; -67.0; 45.0|])) (17.0)) then
            failwith ("interquartile_range case2 failed")
        if not (float_equal (interquartile_range (unbox<float array> [|-2.1; -7.1; -10.1; 9.1; 8.1; 4.1; -67.1; 45.1|])) (17.2)) then
            failwith ("interquartile_range case3 failed")
        if not (float_equal (interquartile_range (unbox<float array> [|0.0; 0.0; 0.0; 0.0; 0.0|])) (0.0)) then
            failwith ("interquartile_range case4 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_interquartile_range()
        printfn "%s" (_str (interquartile_range (unbox<float array> [|4.0; 1.0; 2.0; 3.0; 2.0|])))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
