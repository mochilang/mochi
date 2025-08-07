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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
type TreeNode = {
    data: int
    left: int
    right: int
}
let rec count_nodes (nodes: TreeNode array) (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nodes = nodes
    let mutable idx = idx
    try
        if idx = 0 then
            __ret <- 0
            raise Return
        let node: TreeNode = _idx nodes (idx)
        __ret <- ((count_nodes (nodes) (node.left)) + (count_nodes (nodes) (node.right))) + 1
        raise Return
        __ret
    with
        | Return -> __ret
and count_coins (nodes: TreeNode array) (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nodes = nodes
    let mutable idx = idx
    try
        if idx = 0 then
            __ret <- 0
            raise Return
        let node: TreeNode = _idx nodes (idx)
        __ret <- ((count_coins (nodes) (node.left)) + (count_coins (nodes) (node.right))) + (node.data)
        raise Return
        __ret
    with
        | Return -> __ret
let mutable total_moves: int = 0
let rec iabs (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x < 0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and dfs (nodes: TreeNode array) (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nodes = nodes
    let mutable idx = idx
    try
        if idx = 0 then
            __ret <- 0
            raise Return
        let node: TreeNode = _idx nodes (idx)
        let left_excess: int = dfs (nodes) (node.left)
        let right_excess: int = dfs (nodes) (node.right)
        let abs_left: int = iabs (left_excess)
        let abs_right: int = iabs (right_excess)
        total_moves <- (total_moves + abs_left) + abs_right
        __ret <- (((node.data) + left_excess) + right_excess) - 1
        raise Return
        __ret
    with
        | Return -> __ret
and distribute_coins (nodes: TreeNode array) (root: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nodes = nodes
    let mutable root = root
    try
        if root = 0 then
            __ret <- 0
            raise Return
        if (count_nodes (nodes) (root)) <> (count_coins (nodes) (root)) then
            failwith ("The nodes number should be same as the number of coins")
        total_moves <- 0
        dfs (nodes) (root)
        __ret <- total_moves
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let example1: TreeNode array = [|{ data = 0; left = 0; right = 0 }; { data = 3; left = 2; right = 3 }; { data = 0; left = 0; right = 0 }; { data = 0; left = 0; right = 0 }|]
        let example2: TreeNode array = [|{ data = 0; left = 0; right = 0 }; { data = 0; left = 2; right = 3 }; { data = 3; left = 0; right = 0 }; { data = 0; left = 0; right = 0 }|]
        let example3: TreeNode array = [|{ data = 0; left = 0; right = 0 }; { data = 0; left = 2; right = 3 }; { data = 0; left = 0; right = 0 }; { data = 3; left = 0; right = 0 }|]
        printfn "%d" (distribute_coins (example1) (1))
        printfn "%d" (distribute_coins (example2) (1))
        printfn "%d" (distribute_coins (example3) (1))
        printfn "%d" (distribute_coins (unbox<TreeNode array> [|{ data = 0; left = 0; right = 0 }|]) (0))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
