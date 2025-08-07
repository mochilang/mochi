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
    start: int
    ``end``: int
    ``val``: int
    mid: int
    left: int
    right: int
}
type BuildResult = {
    nodes: Node array
    idx: int
}
type SegmentTree = {
    arr: int array
    op: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec combine (a: int) (b: int) (op: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    let mutable op = op
    try
        if op = 0 then
            __ret <- a + b
            raise Return
        if op = 1 then
            if a > b then
                __ret <- a
                raise Return
            __ret <- b
            raise Return
        if a < b then
            __ret <- a
            raise Return
        __ret <- b
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_tree (nodes: Node array) (arr: int array) (start: int) (``end``: int) (op: int) =
    let mutable __ret : BuildResult = Unchecked.defaultof<BuildResult>
    let mutable nodes = nodes
    let mutable arr = arr
    let mutable start = start
    let mutable ``end`` = ``end``
    let mutable op = op
    try
        if start = ``end`` then
            let node: Node = { start = start; ``end`` = ``end``; ``val`` = _idx arr (start); mid = start; left = -1; right = -1 }
            let new_nodes: Node array = Array.append nodes [|node|]
            __ret <- { nodes = new_nodes; idx = (Seq.length (new_nodes)) - 1 }
            raise Return
        let mid: int = (start + ``end``) / 2
        let left_res: BuildResult = build_tree (nodes) (arr) (start) (mid) (op)
        let right_res: BuildResult = build_tree (left_res.nodes) (arr) (mid + 1) (``end``) (op)
        let left_node: Node = _idx (right_res.nodes) (left_res.idx)
        let right_node: Node = _idx (right_res.nodes) (right_res.idx)
        let ``val``: int = combine (left_node.``val``) (right_node.``val``) (op)
        let parent: Node = { start = start; ``end`` = ``end``; ``val`` = ``val``; mid = mid; left = left_res.idx; right = right_res.idx }
        let new_nodes: Node array = Array.append (right_res.nodes) [|parent|]
        __ret <- { nodes = new_nodes; idx = (Seq.length (new_nodes)) - 1 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec new_segment_tree (collection: int array) (op: int) =
    let mutable __ret : SegmentTree = Unchecked.defaultof<SegmentTree>
    let mutable collection = collection
    let mutable op = op
    try
        __ret <- { arr = collection; op = op }
        raise Return
        __ret
    with
        | Return -> __ret
let rec update (tree: SegmentTree) (i: int) (``val``: int) =
    let mutable __ret : SegmentTree = Unchecked.defaultof<SegmentTree>
    let mutable tree = tree
    let mutable i = i
    let mutable ``val`` = ``val``
    try
        let mutable new_arr: int array = [||]
        let mutable idx: int = 0
        while idx < (Seq.length (tree.arr)) do
            if idx = i then
                new_arr <- Array.append new_arr [|``val``|]
            else
                new_arr <- Array.append new_arr [|_idx (tree.arr) (idx)|]
            idx <- idx + 1
        __ret <- { arr = new_arr; op = tree.op }
        raise Return
        __ret
    with
        | Return -> __ret
let rec query_range (tree: SegmentTree) (i: int) (j: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable tree = tree
    let mutable i = i
    let mutable j = j
    try
        let mutable result: int = _idx (tree.arr) (i)
        let mutable idx: int = i + 1
        while idx <= j do
            result <- combine (result) (_idx (tree.arr) (idx)) (tree.op)
            idx <- idx + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec traverse (tree: SegmentTree) =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    let mutable tree = tree
    try
        if (Seq.length (tree.arr)) = 0 then
            __ret <- Array.empty<Node>
            raise Return
        let res: BuildResult = build_tree (Array.empty<Node>) (tree.arr) (0) ((Seq.length (tree.arr)) - 1) (tree.op)
        __ret <- res.nodes
        raise Return
        __ret
    with
        | Return -> __ret
let rec node_to_string (node: Node) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable node = node
    try
        __ret <- ((((("SegmentTreeNode(start=" + (_str (node.start))) + ", end=") + (_str (node.``end``))) + ", val=") + (_str (node.``val``))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_traverse (tree: SegmentTree) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable tree = tree
    try
        let nodes: Node array = traverse (tree)
        let mutable i: int = 0
        while i < (Seq.length (nodes)) do
            printfn "%s" (node_to_string (_idx nodes (i)))
            i <- i + 1
        printfn "%s" ("")
        __ret
    with
        | Return -> __ret
let arr: int array = [|2; 1; 5; 3; 4|]
for op in [|0; 1; 2|] do
    printfn "%s" ("**************************************************")
    let mutable tree: SegmentTree = new_segment_tree (arr) (int op)
    print_traverse (tree)
    tree <- update (tree) (1) (5)
    print_traverse (tree)
    printfn "%d" (query_range (tree) (3) (4))
    printfn "%d" (query_range (tree) (2) (2))
    printfn "%d" (query_range (tree) (1) (3))
    printfn "%s" ("")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
