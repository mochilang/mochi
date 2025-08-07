// Generated 2025-08-07 10:31 +0700

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
type Tree =
    | Empty
    | Node of Tree * int * Tree
let rec dfs (node: Tree) (target: int) (current: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node = node
    let mutable target = target
    let mutable current = current
    try
        __ret <- (match node with
            | Empty -> 0
            | Node(l, v, r) -> ((if (int (current + (int v))) = target then 1 else 0) + (dfs (l) (target) (current + (int v)))) + (dfs (r) (target) (current + (int v))))
        raise Return
        __ret
    with
        | Return -> __ret
and path_sum (node: Tree) (target: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable node = node
    let mutable target = target
    try
        __ret <- (match node with
            | Empty -> 0
            | Node(l, v, r) -> ((dfs (node) (target) (0)) + (path_sum (l) (target))) + (path_sum (r) (target)))
        raise Return
        __ret
    with
        | Return -> __ret
and sample_tree_one () =
    let mutable __ret : Tree = Unchecked.defaultof<Tree>
    try
        __ret <- Node(10, Node(5, Node(3, Node(3, Empty, Empty), Node(-2, Empty, Empty)), Node(2, Empty, Node(1, Empty, Empty))), Node(-3, Empty, Node(11, Empty, Empty)))
        raise Return
        __ret
    with
        | Return -> __ret
and sample_tree_two () =
    let mutable __ret : Tree = Unchecked.defaultof<Tree>
    try
        __ret <- Node(10, Node(5, Node(3, Node(3, Empty, Empty), Node(-2, Empty, Empty)), Node(2, Empty, Node(1, Empty, Empty))), Node(-3, Empty, Node(10, Empty, Empty)))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let tree1: Tree = sample_tree_one()
        printfn "%d" (path_sum (tree1) (8))
        printfn "%d" (path_sum (tree1) (7))
        let tree2: Tree = sample_tree_two()
        printfn "%d" (path_sum (tree2) (8))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
