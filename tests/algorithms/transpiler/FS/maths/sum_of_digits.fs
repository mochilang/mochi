// Generated 2025-08-17 12:28 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec abs_int (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_digits (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable m: int = abs_int (n)
        let mutable res: int = 0
        while m > 0 do
            res <- res + (((m % 10 + 10) % 10))
            m <- _floordiv (int m) (int 10)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_digits_recursion (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable m: int = abs_int (n)
        if m < 10 then
            __ret <- m
            raise Return
        __ret <- (((m % 10 + 10) % 10)) + (sum_of_digits_recursion (_floordiv (int m) (int 10)))
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_digits_compact (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let s: string = _str (abs_int (n))
        let mutable res: int = 0
        let mutable i: int = 0
        while i < (String.length (s)) do
            res <- res + (int (string (s.[i])))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and test_sum_of_digits () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (sum_of_digits (12345)) <> 15 then
            ignore (failwith ("sum_of_digits 12345 failed"))
        if (sum_of_digits (123)) <> 6 then
            ignore (failwith ("sum_of_digits 123 failed"))
        if (sum_of_digits (-123)) <> 6 then
            ignore (failwith ("sum_of_digits -123 failed"))
        if (sum_of_digits (0)) <> 0 then
            ignore (failwith ("sum_of_digits 0 failed"))
        if (sum_of_digits_recursion (12345)) <> 15 then
            ignore (failwith ("recursion 12345 failed"))
        if (sum_of_digits_recursion (123)) <> 6 then
            ignore (failwith ("recursion 123 failed"))
        if (sum_of_digits_recursion (-123)) <> 6 then
            ignore (failwith ("recursion -123 failed"))
        if (sum_of_digits_recursion (0)) <> 0 then
            ignore (failwith ("recursion 0 failed"))
        if (sum_of_digits_compact (12345)) <> 15 then
            ignore (failwith ("compact 12345 failed"))
        if (sum_of_digits_compact (123)) <> 6 then
            ignore (failwith ("compact 123 failed"))
        if (sum_of_digits_compact (-123)) <> 6 then
            ignore (failwith ("compact -123 failed"))
        if (sum_of_digits_compact (0)) <> 0 then
            ignore (failwith ("compact 0 failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_sum_of_digits())
        ignore (printfn "%s" (_str (sum_of_digits (12345))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
