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
let rec count_recursive (array: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable array = array
    let mutable target = target
    try
        if target < 0 then
            __ret <- 0
            raise Return
        if target = 0 then
            __ret <- 1
            raise Return
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (Seq.length (array)) do
            total <- total + (count_recursive (array) (target - (_idx array (i))))
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec combination_sum_iv (array: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable array = array
    let mutable target = target
    try
        __ret <- count_recursive (array) (target)
        raise Return
        __ret
    with
        | Return -> __ret
let rec count_dp (array: int array) (target: int) (dp: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable array = array
    let mutable target = target
    let mutable dp = dp
    try
        if target < 0 then
            __ret <- 0
            raise Return
        if target = 0 then
            __ret <- 1
            raise Return
        if (_idx dp (target)) > (0 - 1) then
            __ret <- _idx dp (target)
            raise Return
        let mutable total: int = 0
        let mutable i: int = 0
        while i < (Seq.length (array)) do
            total <- total + (count_dp (array) (target - (_idx array (i))) (dp))
            i <- i + 1
        dp.[target] <- total
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec combination_sum_iv_dp_array (array: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable array = array
    let mutable target = target
    try
        let mutable dp: int array = make_list (target + 1) (-1)
        __ret <- count_dp (array) (target) (dp)
        raise Return
        __ret
    with
        | Return -> __ret
let rec combination_sum_iv_bottom_up (n: int) (array: int array) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable array = array
    let mutable target = target
    try
        let mutable dp: int array = make_list (target + 1) (0)
        dp.[0] <- 1
        let mutable i: int = 1
        while i <= target do
            let mutable j: int = 0
            while j < n do
                if (i - (_idx array (j))) >= 0 then
                    dp.[i] <- (_idx dp (i)) + (_idx dp (i - (_idx array (j))))
                j <- j + 1
            i <- i + 1
        __ret <- _idx dp (target)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (combination_sum_iv (unbox<int array> [|1; 2; 5|]) (5)))
printfn "%s" (_str (combination_sum_iv_dp_array (unbox<int array> [|1; 2; 5|]) (5)))
printfn "%s" (_str (combination_sum_iv_bottom_up (3) (unbox<int array> [|1; 2; 5|]) (5)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
