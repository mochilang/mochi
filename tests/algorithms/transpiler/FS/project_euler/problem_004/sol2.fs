// Generated 2025-08-23 14:49 +0700

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
let rec _str v =
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_palindrome (num: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable num = num
    try
        if num < (int64 0) then
            __ret <- false
            raise Return
        let mutable n: int64 = num
        let mutable rev: int64 = int64 0
        while n > (int64 0) do
            rev <- (rev * (int64 10)) + (((n % (int64 10) + (int64 10)) % (int64 10)))
            n <- _floordiv64 (int64 n) (int64 (int64 10))
        __ret <- rev = num
        raise Return
        __ret
    with
        | Return -> __ret
and solution (limit: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable limit = limit
    try
        let mutable answer: int64 = int64 0
        let mutable i: int64 = int64 999
        while i >= (int64 100) do
            let mutable j: int64 = int64 999
            while j >= (int64 100) do
                let product: int64 = i * j
                if ((product < limit) && (is_palindrome (product))) && (product > answer) then
                    answer <- product
                j <- j - (int64 1)
            i <- i - (int64 1)
        __ret <- answer
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_str (solution (int64 998001))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
