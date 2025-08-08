// Generated 2025-08-08 17:35 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
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
let rec reverse (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = (Seq.length (xs)) - 1
        while i >= 0 do
            res <- Array.append res [|(_idx xs (i))|]
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and factors_of_a_number (num: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable num = num
    try
        let mutable facs: int array = Array.empty<int>
        if num < 1 then
            __ret <- facs
            raise Return
        let mutable small: int array = Array.empty<int>
        let mutable large: int array = Array.empty<int>
        let mutable i: int = 1
        while (i * i) <= num do
            if (((num % i + i) % i)) = 0 then
                small <- Array.append small [|i|]
                let d: int = _floordiv num i
                if d <> i then
                    large <- Array.append large [|d|]
            i <- i + 1
        facs <- Array.append (small) (reverse (large))
        __ret <- facs
        raise Return
        __ret
    with
        | Return -> __ret
and run_tests () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (factors_of_a_number (1)) <> [|1|] then
            failwith ("case1 failed")
        if (factors_of_a_number (5)) <> [|1; 5|] then
            failwith ("case2 failed")
        if (factors_of_a_number (24)) <> [|1; 2; 3; 4; 6; 8; 12; 24|] then
            failwith ("case3 failed")
        if (factors_of_a_number (-24)) <> [||] then
            failwith ("case4 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        run_tests()
        printfn "%s" (_str (factors_of_a_number (24)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
