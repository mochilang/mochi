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
    | :? float as f -> sprintf "%.15g" f
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
let rec prime_factors (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 2 then
            __ret <- Array.empty<int>
            raise Return
        let mutable num: int = n
        let mutable i: int = 2
        let mutable factors: int array = Array.empty<int>
        while (i * i) <= num do
            if (((num % i + i) % i)) = 0 then
                factors <- Array.append factors [|i|]
                num <- _floordiv (int num) (int i)
            else
                i <- i + 1
        if num > 1 then
            factors <- Array.append factors [|num|]
        __ret <- factors
        raise Return
        __ret
    with
        | Return -> __ret
and list_eq (a: int array) (b: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            if (_idx a (int i)) <> (_idx b (int i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and test_prime_factors () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if not (list_eq (prime_factors (0)) (Array.empty<int>)) then
            ignore (failwith ("prime_factors(0) failed"))
        if not (list_eq (prime_factors (100)) (unbox<int array> [|2; 2; 5; 5|])) then
            ignore (failwith ("prime_factors(100) failed"))
        if not (list_eq (prime_factors (2560)) (unbox<int array> [|2; 2; 2; 2; 2; 2; 2; 2; 2; 5|])) then
            ignore (failwith ("prime_factors(2560) failed"))
        if not (list_eq (prime_factors (97)) (unbox<int array> [|97|])) then
            ignore (failwith ("prime_factors(97) failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_prime_factors())
        ignore (printfn "%s" (_str (prime_factors (100))))
        ignore (printfn "%s" (_str (prime_factors (2560))))
        ignore (printfn "%s" (_str (prime_factors (97))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
