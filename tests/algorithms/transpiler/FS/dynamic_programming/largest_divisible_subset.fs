// Generated 2025-08-25 08:35 +0700

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
    match box v with
    | :? float as f ->
        if f = floor f then sprintf "%g.0" f else sprintf "%g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
let rec sort_list (nums: int64 array) =
    let mutable __ret : int64 array = Unchecked.defaultof<int64 array>
    let mutable nums = nums
    try
        let mutable arr: int64 array = nums
        let mutable i: int64 = int64 1
        while i < (int64 (Seq.length (arr))) do
            let key: int64 = _idx arr (int i)
            let mutable j: int64 = i - (int64 1)
            while (j >= (int64 0)) && ((_idx arr (int j)) > key) do
                arr.[int (j + (int64 1))] <- _idx arr (int j)
                j <- j - (int64 1)
            arr.[int (j + (int64 1))] <- key
            i <- i + (int64 1)
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and largest_divisible_subset (items: int64 array) =
    let mutable __ret : int64 array = Unchecked.defaultof<int64 array>
    let mutable items = items
    try
        if (Seq.length (items)) = 0 then
            __ret <- Array.empty<int64>
            raise Return
        let mutable nums: int64 array = sort_list (items)
        let n: int64 = int64 (Seq.length (nums))
        let mutable memo: int64 array = Array.empty<int64>
        let mutable prev: int64 array = Array.empty<int64>
        let mutable i: int64 = int64 0
        while i < n do
            memo <- Array.append memo [|int64 (1)|]
            prev <- Array.append prev [|i|]
            i <- i + (int64 1)
        i <- int64 0
        while i < n do
            let mutable j: int64 = int64 0
            while j < i do
                if (((_idx nums (int j)) = (int64 0)) || (((((_idx nums (int i)) % (_idx nums (int j)) + (_idx nums (int j))) % (_idx nums (int j)))) = (int64 0))) && (((_idx memo (int j)) + (int64 1)) > (_idx memo (int i))) then
                    memo.[int i] <- (_idx memo (int j)) + (int64 1)
                    prev.[int i] <- j
                j <- j + (int64 1)
            i <- i + (int64 1)
        let mutable ans: int64 = int64 (0 - 1)
        let mutable last_index: int64 = int64 (0 - 1)
        i <- int64 0
        while i < n do
            if (_idx memo (int i)) > ans then
                ans <- _idx memo (int i)
                last_index <- i
            i <- i + (int64 1)
        if last_index = (int64 (0 - 1)) then
            __ret <- Array.empty<int64>
            raise Return
        let mutable result: int64 array = unbox<int64 array> [|_idx nums (int last_index)|]
        while (_idx prev (int last_index)) <> last_index do
            last_index <- _idx prev (int last_index)
            result <- Array.append result [|(_idx nums (int last_index))|]
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let items: int array = unbox<int array> [|1; 16; 7; 8; 4|]
        let subset: int64 array = largest_divisible_subset (Array.map int64 items)
        ignore (printfn "%s" (((("The longest divisible subset of " + (_str (items))) + " is ") + (_str (subset))) + "."))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
