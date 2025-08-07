// Generated 2025-08-07 15:46 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec sort_list (nums: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nums = nums
    try
        let mutable arr: int array = nums
        let mutable i: int = 1
        while i < (Seq.length (arr)) do
            let key: int = _idx arr (i)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx arr (j)) > key) do
                arr.[j + 1] <- _idx arr (j)
                j <- j - 1
            arr.[j + 1] <- key
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and largest_divisible_subset (items: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable items = items
    try
        if (Seq.length (items)) = 0 then
            __ret <- Array.empty<int>
            raise Return
        let mutable nums: int array = sort_list (items)
        let n: int = Seq.length (nums)
        let mutable memo: int array = [||]
        let mutable prev: int array = [||]
        let mutable i: int = 0
        while i < n do
            memo <- Array.append memo [|1|]
            prev <- Array.append prev [|i|]
            i <- i + 1
        i <- 0
        while i < n do
            let mutable j: int = 0
            while j < i do
                if (((_idx nums (j)) = 0) || (((((_idx nums (i)) % (_idx nums (j)) + (_idx nums (j))) % (_idx nums (j)))) = 0)) && (((_idx memo (j)) + 1) > (_idx memo (i))) then
                    memo.[i] <- (_idx memo (j)) + 1
                    prev.[i] <- j
                j <- j + 1
            i <- i + 1
        let mutable ans: int = 0 - 1
        let mutable last_index: int = 0 - 1
        i <- 0
        while i < n do
            if (_idx memo (i)) > ans then
                ans <- _idx memo (i)
                last_index <- i
            i <- i + 1
        if last_index = (0 - 1) then
            __ret <- Array.empty<int>
            raise Return
        let mutable result: int array = [|_idx nums (last_index)|]
        while (_idx prev (last_index)) <> last_index do
            last_index <- _idx prev (last_index)
            result <- Array.append result [|_idx nums (last_index)|]
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let items: int array = [|1; 16; 7; 8; 4|]
        let subset: int array = largest_divisible_subset (items)
        printfn "%s" (((("The longest divisible subset of " + (_str (items))) + " is ") + (_str (subset))) + ".")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
