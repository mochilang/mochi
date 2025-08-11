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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec is_sorted (arr: int array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable arr = arr
    try
        let mutable i: int = 1
        while i < (Seq.length (arr)) do
            if (_idx arr (int (i - 1))) > (_idx arr (int i)) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and binary_search (sorted_collection: int array) (item: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable sorted_collection = sorted_collection
    let mutable item = item
    try
        if not (is_sorted (sorted_collection)) then
            __ret <- -1
            raise Return
        let mutable left: int = 0
        let mutable right: int = (Seq.length (sorted_collection)) - 1
        while left <= right do
            let midpoint: int = left + (_floordiv (right - left) 2)
            let current_item: int = _idx sorted_collection (int midpoint)
            if current_item = item then
                __ret <- midpoint
                raise Return
            if item < current_item then
                right <- midpoint - 1
            else
                left <- midpoint + 1
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and binary_search_by_recursion (sorted_collection: int array) (item: int) (left: int) (right: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable sorted_collection = sorted_collection
    let mutable item = item
    let mutable left = left
    let mutable right = right
    try
        if right < left then
            __ret <- -1
            raise Return
        let midpoint: int = left + (_floordiv (right - left) 2)
        if (_idx sorted_collection (int midpoint)) = item then
            __ret <- midpoint
            raise Return
        if (_idx sorted_collection (int midpoint)) > item then
            __ret <- binary_search_by_recursion (sorted_collection) (item) (left) (midpoint - 1)
            raise Return
        __ret <- binary_search_by_recursion (sorted_collection) (item) (midpoint + 1) (right)
        raise Return
        __ret
    with
        | Return -> __ret
and exponential_search (sorted_collection: int array) (item: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable sorted_collection = sorted_collection
    let mutable item = item
    try
        if not (is_sorted (sorted_collection)) then
            __ret <- -1
            raise Return
        if (Seq.length (sorted_collection)) = 0 then
            __ret <- -1
            raise Return
        let mutable bound: int = 1
        while (bound < (Seq.length (sorted_collection))) && ((_idx sorted_collection (int bound)) < item) do
            bound <- int ((int64 bound) * (int64 2))
        let mutable left: int = _floordiv bound 2
        let mutable right: int = Array.min ([|bound; (Seq.length (sorted_collection)) - 1|])
        __ret <- binary_search_by_recursion (sorted_collection) (item) (left) (right)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let data: int array = unbox<int array> [|0; 5; 7; 10; 15|]
        printfn "%s" (_str (binary_search (data) (0)))
        printfn "%s" (_str (binary_search (data) (15)))
        printfn "%s" (_str (binary_search (data) (5)))
        printfn "%s" (_str (binary_search (data) (6)))
        printfn "%s" (_str (binary_search_by_recursion (data) (0) (0) ((Seq.length (data)) - 1)))
        printfn "%s" (_str (binary_search_by_recursion (data) (15) (0) ((Seq.length (data)) - 1)))
        printfn "%s" (_str (binary_search_by_recursion (data) (5) (0) ((Seq.length (data)) - 1)))
        printfn "%s" (_str (binary_search_by_recursion (data) (6) (0) ((Seq.length (data)) - 1)))
        printfn "%s" (_str (exponential_search (data) (0)))
        printfn "%s" (_str (exponential_search (data) (15)))
        printfn "%s" (_str (exponential_search (data) (5)))
        printfn "%s" (_str (exponential_search (data) (6)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
