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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let rec comb_sort (data: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable data = data
    try
        let shrink_factor: float = 1.3
        let mutable gap: int = Seq.length (data)
        let mutable completed: bool = false
        while not completed do
            gap <- int ((float gap) / shrink_factor)
            if gap <= 1 then
                gap <- 1
                completed <- true
            let mutable index: int = 0
            while (index + gap) < (Seq.length (data)) do
                if (_idx data (int index)) > (_idx data (int (index + gap))) then
                    let tmp: int = _idx data (int index)
                    data.[int index] <- _idx data (int (index + gap))
                    data.[int (index + gap)] <- tmp
                    completed <- false
                index <- index + 1
        __ret <- data
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_repr (comb_sort (unbox<int array> [|0; 5; 3; 2; 2|])))
        printfn "%s" (_repr (comb_sort (Array.empty<int>)))
        printfn "%s" (_repr (comb_sort (unbox<int array> [|99; 45; -7; 8; 2; 0; -15; 3|])))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
