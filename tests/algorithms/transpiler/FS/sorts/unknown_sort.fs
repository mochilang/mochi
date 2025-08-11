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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec list_min (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable i: int = 1
        let mutable m: int = _idx xs (int 0)
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) < m then
                m <- _idx xs (int i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and list_max (xs: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable xs = xs
    try
        let mutable i: int = 1
        let mutable m: int = _idx xs (int 0)
        while i < (Seq.length (xs)) do
            if (_idx xs (int i)) > m then
                m <- _idx xs (int i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and remove_once (xs: int array) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable value = value
    try
        let mutable res: int array = Array.empty<int>
        let mutable removed: bool = false
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            if (not removed) && ((_idx xs (int i)) = value) then
                removed <- true
            else
                res <- Array.append res [|(_idx xs (int i))|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and reverse_list (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = (Seq.length (xs)) - 1
        while i >= 0 do
            res <- Array.append res [|(_idx xs (int i))|]
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and merge_sort (collection: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    try
        let mutable start: int array = Array.empty<int>
        let mutable ``end``: int array = Array.empty<int>
        let mutable coll: int array = collection
        while (Seq.length (coll)) > 1 do
            let mn: int = list_min (coll)
            let mx: int = list_max (coll)
            start <- Array.append start [|mn|]
            ``end`` <- Array.append ``end`` [|mx|]
            coll <- remove_once (coll) (mn)
            coll <- remove_once (coll) (mx)
        ``end`` <- reverse_list (``end``)
        __ret <- Array.append (Array.append (start) (coll)) (``end``)
        raise Return
        __ret
    with
        | Return -> __ret
and test_merge_sort () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (merge_sort (unbox<int array> [|0; 5; 3; 2; 2|])) <> [|0; 2; 2; 3; 5|] then
            failwith ("case1 failed")
        if (merge_sort (Array.empty<int>)) <> [||] then
            failwith ("case2 failed")
        if (merge_sort (unbox<int array> [|-2; -5; -45|])) <> [|-45; -5; -2|] then
            failwith ("case3 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_merge_sort()
        printfn "%s" (_str (merge_sort (unbox<int array> [|0; 5; 3; 2; 2|])))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
