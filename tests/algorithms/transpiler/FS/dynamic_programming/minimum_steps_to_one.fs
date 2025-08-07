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
let rec min_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
let rec min_steps_to_one (number: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable number = number
    try
        if number <= 0 then
            __ret <- 0
            raise Return
        let mutable table: int array = make_list (number + 1) (number + 1)
        table.[1] <- 0
        let mutable i: int = 1
        while i < number do
            table.[i + 1] <- min_int (_idx table (i + 1)) ((_idx table (i)) + 1)
            if (i * 2) <= number then
                table.[i * 2] <- min_int (_idx table (i * 2)) ((_idx table (i)) + 1)
            if (i * 3) <= number then
                table.[i * 3] <- min_int (_idx table (i * 3)) ((_idx table (i)) + 1)
            i <- i + 1
        __ret <- _idx table (number)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (min_steps_to_one (10)))
printfn "%s" (_str (min_steps_to_one (15)))
printfn "%s" (_str (min_steps_to_one (6)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
