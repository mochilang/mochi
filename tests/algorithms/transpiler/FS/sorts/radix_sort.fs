// Generated 2025-08-11 17:23 +0700

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
let RADIX: int = 10
let rec make_buckets () =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    try
        let mutable buckets: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < RADIX do
            buckets <- Array.append buckets [|[||]|]
            i <- i + 1
        __ret <- buckets
        raise Return
        __ret
    with
        | Return -> __ret
and max_value (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable max_val: int = _idx xs (int 0)
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) > max_val then
                max_val <- _idx xs (int i)
            i <- i + 1
        __ret <- max_val
        raise Return
        __ret
    with
        | Return -> __ret
and radix_sort (list_of_ints: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable list_of_ints = list_of_ints
    try
        let mutable placement: int = 1
        let max_digit: int = max_value (list_of_ints)
        while placement <= max_digit do
            let mutable buckets: int array array = make_buckets()
            let mutable i: int = 0
            while i < (Seq.length (list_of_ints)) do
                let value: int = _idx list_of_ints (int i)
                let tmp: int = (((_floordiv value placement) % RADIX + RADIX) % RADIX)
                buckets.[int tmp] <- Array.append (_idx buckets (int tmp)) [|value|]
                i <- i + 1
            let mutable a: int = 0
            let mutable b: int = 0
            while b < RADIX do
                let mutable bucket: int array = _idx buckets (int b)
                let mutable j: int = 0
                while j < (Seq.length (bucket)) do
                    list_of_ints.[int a] <- _idx bucket (int j)
                    a <- a + 1
                    j <- j + 1
                b <- b + 1
            placement <- int ((int64 placement) * (int64 RADIX))
        __ret <- list_of_ints
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (radix_sort (unbox<int array> [|0; 5; 3; 2; 2|])))
printfn "%s" (_str (radix_sort (unbox<int array> [|1; 100; 10; 1000|])))
printfn "%s" (_str (radix_sort (unbox<int array> [|15; 14; 13; 12; 11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
