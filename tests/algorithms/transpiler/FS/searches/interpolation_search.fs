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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec interpolation_search (arr: int array) (item: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable item = item
    try
        let mutable left: int = 0
        let mutable right: int = (Seq.length (arr)) - 1
        while left <= right do
            if (_idx arr (int left)) = (_idx arr (int right)) then
                if (_idx arr (int left)) = item then
                    __ret <- left
                    raise Return
                __ret <- -1
                raise Return
            let point: int64 = (int64 left) + (((int64 (item - (_idx arr (int left)))) * (int64 (right - left))) / (int64 ((_idx arr (int right)) - (_idx arr (int left)))))
            if (point < (int64 0)) || (point >= (int64 (Seq.length (arr)))) then
                __ret <- -1
                raise Return
            let current: int = _idx arr (int point)
            if current = item then
                __ret <- int point
                raise Return
            if point < (int64 left) then
                right <- left
                left <- int point
            else
                if point > (int64 right) then
                    left <- right
                    right <- int point
                else
                    if item < current then
                        right <- int (point - (int64 1))
                    else
                        left <- int (point + (int64 1))
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec interpolation_search_recursive (arr: int array) (item: int) (left: int) (right: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable item = item
    let mutable left = left
    let mutable right = right
    try
        if left > right then
            __ret <- -1
            raise Return
        if (_idx arr (int left)) = (_idx arr (int right)) then
            if (_idx arr (int left)) = item then
                __ret <- left
                raise Return
            __ret <- -1
            raise Return
        let point: int64 = (int64 left) + (((int64 (item - (_idx arr (int left)))) * (int64 (right - left))) / (int64 ((_idx arr (int right)) - (_idx arr (int left)))))
        if (point < (int64 0)) || (point >= (int64 (Seq.length (arr)))) then
            __ret <- -1
            raise Return
        if (_idx arr (int point)) = item then
            __ret <- int point
            raise Return
        if point < (int64 left) then
            __ret <- interpolation_search_recursive (arr) (item) (point) (left)
            raise Return
        if point > (int64 right) then
            __ret <- interpolation_search_recursive (arr) (item) (right) (left)
            raise Return
        if (_idx arr (int point)) > item then
            __ret <- interpolation_search_recursive (arr) (item) (left) (point - (int64 1))
            raise Return
        __ret <- interpolation_search_recursive (arr) (item) (point + (int64 1)) (right)
        raise Return
        __ret
    with
        | Return -> __ret
let rec interpolation_search_by_recursion (arr: int array) (item: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable item = item
    try
        __ret <- interpolation_search_recursive (arr) (item) (0) ((Seq.length (arr)) - 1)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (interpolation_search (unbox<int array> [|1; 2; 3; 4; 5|]) (2)))
printfn "%s" (_str (interpolation_search (unbox<int array> [|1; 2; 3; 4; 5|]) (6)))
printfn "%s" (_str (interpolation_search_by_recursion (unbox<int array> [|0; 5; 7; 10; 15|]) (5)))
printfn "%s" (_str (interpolation_search_by_recursion (unbox<int array> [|0; 5; 7; 10; 15|]) (100)))
printfn "%s" (_str (interpolation_search_by_recursion (unbox<int array> [|5; 5; 5; 5; 5|]) (3)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
