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
let rec cycle_sort (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        let mutable n: int = Seq.length (arr)
        let mutable cycle_start: int = 0
        try
            while cycle_start < (n - 1) do
                try
                    let mutable item: int = _idx arr (int cycle_start)
                    let mutable pos: int = cycle_start
                    let mutable i: int = cycle_start + 1
                    while i < n do
                        if (_idx arr (int i)) < item then
                            pos <- pos + 1
                        i <- i + 1
                    if pos = cycle_start then
                        cycle_start <- cycle_start + 1
                        raise Continue
                    while item = (_idx arr (int pos)) do
                        pos <- pos + 1
                    let temp: int = _idx arr (int pos)
                    arr.[int pos] <- item
                    item <- temp
                    while pos <> cycle_start do
                        pos <- cycle_start
                        i <- cycle_start + 1
                        while i < n do
                            if (_idx arr (int i)) < item then
                                pos <- pos + 1
                            i <- i + 1
                        while item = (_idx arr (int pos)) do
                            pos <- pos + 1
                        let temp2: int = _idx arr (int pos)
                        arr.[int pos] <- item
                        item <- temp2
                    cycle_start <- cycle_start + 1
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
printfn "%s" (_str (cycle_sort (unbox<int array> [|4; 3; 2; 1|])))
printfn "%s" (_str (cycle_sort (unbox<int array> [|-4; 20; 0; -50; 100; -1|])))
printfn "%s" (_str (cycle_sort (Array.empty<int>)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
