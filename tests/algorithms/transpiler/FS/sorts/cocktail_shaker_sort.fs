// Generated 2025-08-11 16:20 +0700

exception Break
exception Continue

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec cocktail_shaker_sort (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        let mutable start: int = 0
        let mutable ``end``: int = (Seq.length (arr)) - 1
        try
            while start < ``end`` do
                try
                    let mutable swapped: bool = false
                    let mutable i: int = start
                    while i < ``end`` do
                        if (_idx arr (int i)) > (_idx arr (int (i + 1))) then
                            let temp: int = _idx arr (int i)
                            arr.[int i] <- _idx arr (int (i + 1))
                            arr.[int (i + 1)] <- temp
                            swapped <- true
                        i <- i + 1
                    if not swapped then
                        raise Break
                    ``end`` <- ``end`` - 1
                    i <- ``end``
                    while i > start do
                        if (_idx arr (int i)) < (_idx arr (int (i - 1))) then
                            let temp2: int = _idx arr (int i)
                            arr.[int i] <- _idx arr (int (i - 1))
                            arr.[int (i - 1)] <- temp2
                            swapped <- true
                        i <- i - 1
                    if not swapped then
                        raise Break
                    start <- start + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (cocktail_shaker_sort (unbox<int array> [|4; 5; 2; 1; 2|])))
printfn "%s" (_str (cocktail_shaker_sort (unbox<int array> [|-4; 5; 0; 1; 2; 11|])))
printfn "%s" (_str (cocktail_shaker_sort (unbox<int array> [|1; 2; 3; 4; 5|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
