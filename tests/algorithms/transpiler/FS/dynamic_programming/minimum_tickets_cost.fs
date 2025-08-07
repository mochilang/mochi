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
let rec max_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if a > b then
            __ret <- a
            raise Return
        else
            __ret <- b
            raise Return
        __ret
    with
        | Return -> __ret
let rec min_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        if a < b then
            __ret <- a
            raise Return
        else
            __ret <- b
            raise Return
        __ret
    with
        | Return -> __ret
let rec min3 (a: int) (b: int) (c: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        __ret <- min_int (min_int (a) (b)) (c)
        raise Return
        __ret
    with
        | Return -> __ret
let rec minimum_tickets_cost (days: int array) (costs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable days = days
    let mutable costs = costs
    try
        if (Seq.length (days)) = 0 then
            __ret <- 0
            raise Return
        let mutable last_day: int = _idx days ((Seq.length (days)) - 1)
        let mutable dp: int array = make_list (last_day + 1) (0)
        let mutable day_index: int = 0
        let mutable d: int = 1
        while d <= last_day do
            if (day_index < (Seq.length (days))) && (d = (_idx days (day_index))) then
                let cost1: int = (_idx dp (d - 1)) + (_idx costs (0))
                let cost7: int = (_idx dp (max_int (0) (d - 7))) + (_idx costs (1))
                let cost30: int = (_idx dp (max_int (0) (d - 30))) + (_idx costs (2))
                dp.[d] <- min3 (cost1) (cost7) (cost30)
                day_index <- day_index + 1
            else
                dp.[d] <- _idx dp (d - 1)
            d <- d + 1
        __ret <- _idx dp (last_day)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (minimum_tickets_cost (unbox<int array> [|1; 4; 6; 7; 8; 20|]) (unbox<int array> [|2; 7; 15|])))
printfn "%s" (_str (minimum_tickets_cost (unbox<int array> [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 30; 31|]) (unbox<int array> [|2; 7; 15|])))
printfn "%s" (_str (minimum_tickets_cost (unbox<int array> [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 30; 31|]) (unbox<int array> [|2; 90; 150|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
