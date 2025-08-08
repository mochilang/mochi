// Generated 2025-08-08 18:58 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let rec two_pointer (nums: int array) (target: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    let mutable target = target
    try
        let mutable i: int = 0
        let mutable j: int = (Seq.length (nums)) - 1
        while i < j do
            let s: int = (_idx nums (int i)) + (_idx nums (int j))
            if s = target then
                __ret <- unbox<int array> [|i; j|]
                raise Return
            if s < target then
                i <- i + 1
            else
                j <- j - 1
        __ret <- Array.empty<int>
        raise Return
        __ret
    with
        | Return -> __ret
and test_two_pointer () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (two_pointer (unbox<int array> [|2; 7; 11; 15|]) (9)) <> [|0; 1|] then
            failwith ("case1")
        if (two_pointer (unbox<int array> [|2; 7; 11; 15|]) (17)) <> [|0; 3|] then
            failwith ("case2")
        if (two_pointer (unbox<int array> [|2; 7; 11; 15|]) (18)) <> [|1; 2|] then
            failwith ("case3")
        if (two_pointer (unbox<int array> [|2; 7; 11; 15|]) (26)) <> [|2; 3|] then
            failwith ("case4")
        if (two_pointer (unbox<int array> [|1; 3; 3|]) (6)) <> [|1; 2|] then
            failwith ("case5")
        if (Seq.length (two_pointer (unbox<int array> [|2; 7; 11; 15|]) (8))) <> 0 then
            failwith ("case6")
        if (Seq.length (two_pointer (unbox<int array> [|0; 3; 6; 9; 12; 15; 18; 21; 24; 27|]) (19))) <> 0 then
            failwith ("case7")
        if (Seq.length (two_pointer (unbox<int array> [|1; 2; 3|]) (6))) <> 0 then
            failwith ("case8")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_two_pointer()
        printfn "%s" (_repr (two_pointer (unbox<int array> [|2; 7; 11; 15|]) (9)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
