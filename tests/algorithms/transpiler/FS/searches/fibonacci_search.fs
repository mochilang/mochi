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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec fibonacci (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable k = k
    try
        if k < 0 then
            failwith ("k must be >= 0")
        let mutable a: int = 0
        let mutable b: int = 1
        let mutable i: int = 0
        while i < k do
            let tmp: int = a + b
            a <- b
            b <- tmp
            i <- i + 1
        __ret <- a
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
let rec fibonacci_search (arr: int array) (``val``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable ``val`` = ``val``
    try
        let n: int = Seq.length (arr)
        let mutable m: int = 0
        while (fibonacci (m)) < n do
            m <- m + 1
        let mutable offset: int = 0
        while m > 0 do
            let mutable i: int = min_int (offset + (fibonacci (m - 1))) (n - 1)
            let item: int = _idx arr (int i)
            if item = ``val`` then
                __ret <- i
                raise Return
            else
                if ``val`` < item then
                    m <- m - 1
                else
                    offset <- offset + (fibonacci (m - 1))
                    m <- m - 2
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let example1: int array = unbox<int array> [|4; 5; 6; 7|]
let example2: int array = unbox<int array> [|-18; 2|]
let example3: int array = unbox<int array> [|0; 5; 10; 15; 20; 25; 30|]
printfn "%s" (_str (fibonacci_search (example1) (4)))
printfn "%s" (_str (fibonacci_search (example1) (-10)))
printfn "%s" (_str (fibonacci_search (example2) (-18)))
printfn "%s" (_str (fibonacci_search (example3) (15)))
printfn "%s" (_str (fibonacci_search (example3) (17)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
