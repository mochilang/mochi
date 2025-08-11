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
let rec int_sqrt (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable x: int = 0
        while ((int64 (x + 1)) * (int64 (x + 1))) <= (int64 n) do
            x <- x + 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and jump_search (arr: int array) (item: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable item = item
    try
        let arr_size: int = Seq.length (arr)
        let block_size: int = int_sqrt (arr_size)
        let mutable prev: int = 0
        let mutable step: int = block_size
        while (step < arr_size) && ((_idx arr (int (step - 1))) < item) do
            prev <- step
            step <- step + block_size
            if prev >= arr_size then
                __ret <- -1
                raise Return
        while (prev < arr_size) && ((_idx arr (int prev)) < item) do
            prev <- prev + 1
            if prev = step then
                __ret <- -1
                raise Return
        if (prev < arr_size) && ((_idx arr (int prev)) = item) then
            __ret <- prev
            raise Return
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
        printfn "%s" (_str (jump_search (unbox<int array> [|0; 1; 2; 3; 4; 5|]) (3)))
        printfn "%s" (_str (jump_search (unbox<int array> [|-5; -2; -1|]) (-1)))
        printfn "%s" (_str (jump_search (unbox<int array> [|0; 5; 10; 20|]) (8)))
        printfn "%s" (_str (jump_search (unbox<int array> [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610|]) (55)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
