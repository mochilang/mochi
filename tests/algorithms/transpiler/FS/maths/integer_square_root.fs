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
let rec integer_square_root (num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    try
        if num < 0 then
            failwith ("num must be non-negative integer")
        if num < 2 then
            __ret <- num
            raise Return
        let mutable left_bound: int = 0
        let mutable right_bound: int = _floordiv num 2
        while left_bound <= right_bound do
            let mid: int = left_bound + (_floordiv (right_bound - left_bound) 2)
            let mid_squared: int64 = (int64 mid) * (int64 mid)
            if mid_squared = (int64 num) then
                __ret <- mid
                raise Return
            if mid_squared < (int64 num) then
                left_bound <- mid + 1
            else
                right_bound <- mid - 1
        __ret <- right_bound
        raise Return
        __ret
    with
        | Return -> __ret
and test_integer_square_root () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let expected: int array = unbox<int array> [|0; 1; 1; 1; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 4; 4|]
        let mutable i: int = 0
        while i < (Seq.length (expected)) do
            let result: int = integer_square_root (i)
            if result <> (_idx expected (int i)) then
                failwith ("test failed at index " + (_str (i)))
            i <- i + 1
        if (integer_square_root (625)) <> 25 then
            failwith ("sqrt of 625 incorrect")
        if (integer_square_root (2147483647)) <> 46340 then
            failwith ("sqrt of max int incorrect")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_integer_square_root()
        printfn "%s" (_str (integer_square_root (625)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
