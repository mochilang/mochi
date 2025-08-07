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
type Node = {
    value: int
    left: int
    right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec tree_sum (nodes: Node array) (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nodes = nodes
    let mutable idx = idx
    try
        if idx = (-1) then
            __ret <- 0
            raise Return
        let node: Node = _idx nodes (idx)
        __ret <- ((node.value) + (tree_sum (nodes) (node.left))) + (tree_sum (nodes) (node.right))
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_sum_node (nodes: Node array) (idx: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable nodes = nodes
    let mutable idx = idx
    try
        let node: Node = _idx nodes (idx)
        if ((node.left) = (-1)) && ((node.right) = (-1)) then
            __ret <- true
            raise Return
        let left_sum: int = tree_sum (nodes) (node.left)
        let right_sum: int = tree_sum (nodes) (node.right)
        if (node.value) <> (left_sum + right_sum) then
            __ret <- false
            raise Return
        let mutable left_ok: bool = true
        if (node.left) <> (-1) then
            left_ok <- is_sum_node (nodes) (node.left)
        let mutable right_ok: bool = true
        if (node.right) <> (-1) then
            right_ok <- is_sum_node (nodes) (node.right)
        __ret <- left_ok && right_ok
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_a_tree () =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    try
        __ret <- unbox<Node array> [|{ value = 11; left = 1; right = 2 }; { value = 2; left = 3; right = 4 }; { value = 29; left = 5; right = 6 }; { value = 1; left = -1; right = -1 }; { value = 7; left = -1; right = -1 }; { value = 15; left = -1; right = -1 }; { value = 40; left = 7; right = -1 }; { value = 35; left = -1; right = -1 }|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_a_sum_tree () =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    try
        __ret <- unbox<Node array> [|{ value = 26; left = 1; right = 2 }; { value = 10; left = 3; right = 4 }; { value = 3; left = -1; right = 5 }; { value = 4; left = -1; right = -1 }; { value = 6; left = -1; right = -1 }; { value = 3; left = -1; right = -1 }|]
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
