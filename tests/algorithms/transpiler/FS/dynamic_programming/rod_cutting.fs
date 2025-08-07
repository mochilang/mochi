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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec enforce_args (n: int) (prices: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable prices = prices
    try
        if n < 0 then
            failwith ("n must be non-negative")
        if n > (Seq.length (prices)) then
            failwith ("price list is shorter than n")
        __ret
    with
        | Return -> __ret
let rec bottom_up_cut_rod (n: int) (prices: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    let mutable prices = prices
    try
        enforce_args (n) (prices)
        let mutable max_rev: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= n do
            if i = 0 then
                max_rev <- Array.append max_rev [|0|]
            else
                max_rev <- Array.append max_rev [|-2147483648L|]
            i <- i + 1
        let mutable length: int = 1
        while length <= n do
            let mutable best: int = _idx max_rev (length)
            let mutable j: int = 1
            while j <= length do
                let candidate: int = (_idx prices (j - 1)) + (_idx max_rev (length - j))
                if candidate > best then
                    best <- candidate
                j <- j + 1
            max_rev.[length] <- best
            length <- length + 1
        __ret <- _idx max_rev (n)
        raise Return
        __ret
    with
        | Return -> __ret
let prices: int array = [|1; 5; 8; 9; 10; 17; 17; 20; 24; 30|]
printfn "%d" (bottom_up_cut_rod (4) (prices))
printfn "%d" (bottom_up_cut_rod (10) (prices))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
