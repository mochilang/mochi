// Generated 2025-08-07 14:57 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Node = {
    minn: int
    maxx: int
    map_left: int array
    left: int
    right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable nodes: Node array = [||]
let rec make_list (length: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable length = length
    let mutable value = value
    try
        let mutable lst: int array = [||]
        let mutable i: int = 0
        while i < length do
            lst <- Array.append lst [|value|]
            i <- i + 1
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
let rec min_list (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let mutable m: int = _idx arr (0)
        let mutable i: int = 1
        while i < (Seq.length (arr)) do
            if (_idx arr (i)) < m then
                m <- _idx arr (i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec max_list (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let mutable m: int = _idx arr (0)
        let mutable i: int = 1
        while i < (Seq.length (arr)) do
            if (_idx arr (i)) > m then
                m <- _idx arr (i)
            i <- i + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_tree (arr: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    try
        let mutable n: Node = { minn = min_list (arr); maxx = max_list (arr); map_left = make_list (Seq.length (arr)) (0); left = -1; right = -1 }
        if (n.minn) = (n.maxx) then
            nodes <- Array.append nodes [|n|]
            __ret <- (Seq.length (nodes)) - 1
            raise Return
        let pivot: int = ((n.minn) + (n.maxx)) / 2
        let mutable left_arr: int array = [||]
        let mutable right_arr: int array = [||]
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let num: int = _idx arr (i)
            if num <= pivot then
                left_arr <- Array.append left_arr [|num|]
            else
                right_arr <- Array.append right_arr [|num|]
            let mutable ml: int array = n.map_left
            ml.[i] <- Seq.length (left_arr)
            n <- { n with map_left = ml }
            i <- i + 1
        if (Seq.length (left_arr)) > 0 then
            n <- { n with left = build_tree (left_arr) }
        if (Seq.length (right_arr)) > 0 then
            n <- { n with right = build_tree (right_arr) }
        nodes <- Array.append nodes [|n|]
        __ret <- (Seq.length (nodes)) - 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec rank_till_index (node_idx: int) (num: int) (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node_idx = node_idx
    let mutable num = num
    let mutable index = index
    try
        if (index < 0) || (node_idx < 0) then
            __ret <- 0
            raise Return
        let node: Node = _idx nodes (node_idx)
        if (node.minn) = (node.maxx) then
            if (node.minn) = num then
                __ret <- index + 1
                raise Return
            else
                __ret <- 0
                raise Return
        let pivot: int = ((node.minn) + (node.maxx)) / 2
        if num <= pivot then
            __ret <- rank_till_index (node.left) (num) ((_idx (node.map_left) (index)) - 1)
            raise Return
        else
            __ret <- rank_till_index (node.right) (num) (index - (_idx (node.map_left) (index)))
            raise Return
        __ret
    with
        | Return -> __ret
let rec rank (node_idx: int) (num: int) (start: int) (``end``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node_idx = node_idx
    let mutable num = num
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        if start > ``end`` then
            __ret <- 0
            raise Return
        let rank_till_end: int = rank_till_index (node_idx) (num) (``end``)
        let rank_before_start: int = rank_till_index (node_idx) (num) (start - 1)
        __ret <- rank_till_end - rank_before_start
        raise Return
        __ret
    with
        | Return -> __ret
let rec quantile (node_idx: int) (index: int) (start: int) (``end``: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node_idx = node_idx
    let mutable index = index
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        if ((index > (``end`` - start)) || (start > ``end``)) || (node_idx < 0) then
            __ret <- -1
            raise Return
        let node: Node = _idx nodes (node_idx)
        if (node.minn) = (node.maxx) then
            __ret <- node.minn
            raise Return
        let left_start: int = if start = 0 then 0 else (_idx (node.map_left) (start - 1))
        let num_left: int = (_idx (node.map_left) (``end``)) - left_start
        if num_left > index then
            __ret <- quantile (node.left) (index) (left_start) ((_idx (node.map_left) (``end``)) - 1)
            raise Return
        else
            __ret <- quantile (node.right) (index - num_left) (start - left_start) (``end`` - (_idx (node.map_left) (``end``)))
            raise Return
        __ret
    with
        | Return -> __ret
let rec range_counting (node_idx: int) (start: int) (``end``: int) (start_num: int) (end_num: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node_idx = node_idx
    let mutable start = start
    let mutable ``end`` = ``end``
    let mutable start_num = start_num
    let mutable end_num = end_num
    try
        if ((start > ``end``) || (node_idx < 0)) || (start_num > end_num) then
            __ret <- 0
            raise Return
        let node: Node = _idx nodes (node_idx)
        if ((node.minn) > end_num) || ((node.maxx) < start_num) then
            __ret <- 0
            raise Return
        if (start_num <= (node.minn)) && ((node.maxx) <= end_num) then
            __ret <- (``end`` - start) + 1
            raise Return
        let left: int = range_counting (node.left) (if start = 0 then 0 else (_idx (node.map_left) (start - 1))) ((_idx (node.map_left) (``end``)) - 1) (start_num) (end_num)
        let right: int = range_counting (node.right) (start - (if start = 0 then 0 else (_idx (node.map_left) (start - 1)))) (``end`` - (_idx (node.map_left) (``end``))) (start_num) (end_num)
        __ret <- left + right
        raise Return
        __ret
    with
        | Return -> __ret
let test_array: int array = [|2; 1; 4; 5; 6; 0; 8; 9; 1; 2; 0; 6; 4; 2; 0; 6; 5; 3; 2; 7|]
let root: int = build_tree (test_array)
printfn "%s" ("rank_till_index 6 at 6 -> " + (_str (rank_till_index (root) (6) (6))))
printfn "%s" ("rank 6 in [3,13] -> " + (_str (rank (root) (6) (3) (13))))
printfn "%s" ("quantile index 2 in [2,5] -> " + (_str (quantile (root) (2) (2) (5))))
printfn "%s" ("range_counting [3,7] in [1,10] -> " + (_str (range_counting (root) (1) (10) (3) (7))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
