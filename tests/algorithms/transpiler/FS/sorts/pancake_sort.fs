// Generated 2025-08-11 17:23 +0700

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
let rec flip (arr: int array) (k: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable k = k
    try
        let mutable start: int = 0
        let mutable ``end``: int = k
        while start < ``end`` do
            let temp: int = _idx arr (int start)
            arr.[int start] <- _idx arr (int ``end``)
            arr.[int ``end``] <- temp
            start <- start + 1
            ``end`` <- ``end`` - 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and find_max_index (arr: int array) (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable n = n
    try
        let mutable mi: int = 0
        let mutable i: int = 1
        while i < n do
            if (_idx arr (int i)) > (_idx arr (int mi)) then
                mi <- i
            i <- i + 1
        __ret <- mi
        raise Return
        __ret
    with
        | Return -> __ret
and pancake_sort (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        let mutable cur: int = Seq.length (arr)
        while cur > 1 do
            let mutable mi: int = find_max_index (arr) (cur)
            arr <- flip (arr) (mi)
            arr <- flip (arr) (cur - 1)
            cur <- cur - 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable data: int array = unbox<int array> [|3; 6; 1; 10; 2|]
        let mutable sorted: int array = pancake_sort (data)
        printfn "%s" (_str (sorted))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
