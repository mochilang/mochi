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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec double_linear_search (array: int array) (search_item: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable array = array
    let mutable search_item = search_item
    try
        let mutable start_ind: int = 0
        let mutable end_ind: int = (Seq.length (array)) - 1
        while start_ind <= end_ind do
            if (_idx array (int start_ind)) = search_item then
                __ret <- start_ind
                raise Return
            if (_idx array (int end_ind)) = search_item then
                __ret <- end_ind
                raise Return
            start_ind <- start_ind + 1
            end_ind <- end_ind - 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let data: int array = build_range (100)
        printfn "%s" (_str (double_linear_search (data) (40)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
and build_range (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|i|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
main()
