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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec dutch_national_flag_sort (seq: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable seq = seq
    try
        let mutable a: int array = seq
        let mutable low: int = 0
        let mutable mid: int = 0
        let mutable high: int = (Seq.length (a)) - 1
        while mid <= high do
            let v: int = _idx a (int mid)
            if v = 0 then
                let tmp: int = _idx a (int low)
                a.[int low] <- v
                a.[int mid] <- tmp
                low <- low + 1
                mid <- mid + 1
            else
                if v = 1 then
                    mid <- mid + 1
                else
                    if v = 2 then
                        let tmp2: int = _idx a (int high)
                        a.[int high] <- v
                        a.[int mid] <- tmp2
                        high <- high - 1
                    else
                        failwith ("The elements inside the sequence must contains only (0, 1, 2) values")
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (dutch_national_flag_sort (Array.empty<int>)))
printfn "%s" (_repr (dutch_national_flag_sort (unbox<int array> [|0|])))
printfn "%s" (_repr (dutch_national_flag_sort (unbox<int array> [|2; 1; 0; 0; 1; 2|])))
printfn "%s" (_repr (dutch_national_flag_sort (unbox<int array> [|0; 1; 1; 0; 1; 2; 1; 2; 0; 0; 0; 1|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
