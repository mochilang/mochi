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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec hamming (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        if n < 1 then
            ignore (failwith ("n_element should be a positive number"))
        let mutable hamming_list: int array = unbox<int array> [|1|]
        let mutable i: int = 0
        let mutable j: int = 0
        let mutable k: int = 0
        let mutable index: int = 1
        while index < n do
            while ((_idx hamming_list (int i)) * 2) <= (_idx hamming_list (int ((Seq.length (hamming_list)) - 1))) do
                i <- i + 1
            while ((_idx hamming_list (int j)) * 3) <= (_idx hamming_list (int ((Seq.length (hamming_list)) - 1))) do
                j <- j + 1
            while ((_idx hamming_list (int k)) * 5) <= (_idx hamming_list (int ((Seq.length (hamming_list)) - 1))) do
                k <- k + 1
            let m1: int = (_idx hamming_list (int i)) * 2
            let m2: int = (_idx hamming_list (int j)) * 3
            let m3: int = (_idx hamming_list (int k)) * 5
            let mutable next: int = m1
            if m2 < next then
                next <- m2
            if m3 < next then
                next <- m3
            hamming_list <- Array.append hamming_list [|next|]
            index <- index + 1
        __ret <- hamming_list
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_repr (hamming (5))))
ignore (printfn "%s" (_repr (hamming (10))))
ignore (printfn "%s" (_repr (hamming (15))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
