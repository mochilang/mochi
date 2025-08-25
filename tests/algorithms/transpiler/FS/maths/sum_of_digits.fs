// Generated 2025-08-25 22:27 +0700

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
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let rec abs_int (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        __ret <- if n < (int64 0) then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_digits (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable m: int64 = abs_int (int64 n)
        let mutable res: int64 = int64 0
        while m > (int64 0) do
            res <- res + (((m % (int64 10) + (int64 10)) % (int64 10)))
            m <- _floordiv64 (int64 m) (int64 (int64 10))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_digits_recursion (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable m: int64 = abs_int (int64 n)
        if m < (int64 10) then
            __ret <- m
            raise Return
        __ret <- (((m % (int64 10) + (int64 10)) % (int64 10))) + (sum_of_digits_recursion (_floordiv64 (int64 m) (int64 (int64 10))))
        raise Return
        __ret
    with
        | Return -> __ret
and sum_of_digits_compact (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let s: string = _str (abs_int (int64 n))
        let mutable res: int64 = int64 0
        let mutable i: int64 = int64 0
        while i < (int64 (String.length (s))) do
            res <- res + (int64 (string (s.[int i])))
            i <- i + (int64 1)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and test_sum_of_digits () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (sum_of_digits (int64 12345)) <> (int64 15) then
            ignore (failwith ("sum_of_digits 12345 failed"))
        if (sum_of_digits (int64 123)) <> (int64 6) then
            ignore (failwith ("sum_of_digits 123 failed"))
        if (sum_of_digits (int64 (-123))) <> (int64 6) then
            ignore (failwith ("sum_of_digits -123 failed"))
        if (sum_of_digits (int64 0)) <> (int64 0) then
            ignore (failwith ("sum_of_digits 0 failed"))
        if (sum_of_digits_recursion (int64 12345)) <> (int64 15) then
            ignore (failwith ("recursion 12345 failed"))
        if (sum_of_digits_recursion (int64 123)) <> (int64 6) then
            ignore (failwith ("recursion 123 failed"))
        if (sum_of_digits_recursion (int64 (-123))) <> (int64 6) then
            ignore (failwith ("recursion -123 failed"))
        if (sum_of_digits_recursion (int64 0)) <> (int64 0) then
            ignore (failwith ("recursion 0 failed"))
        if (sum_of_digits_compact (int64 12345)) <> (int64 15) then
            ignore (failwith ("compact 12345 failed"))
        if (sum_of_digits_compact (int64 123)) <> (int64 6) then
            ignore (failwith ("compact 123 failed"))
        if (sum_of_digits_compact (int64 (-123))) <> (int64 6) then
            ignore (failwith ("compact -123 failed"))
        if (sum_of_digits_compact (int64 0)) <> (int64 0) then
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
        ignore (printfn "%s" (_str (sum_of_digits (int64 12345))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
