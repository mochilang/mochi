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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
type Node = {
    data: int
    left: int
    right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec inorder (nodes: Node array) (index: int) (acc: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable nodes = nodes
    let mutable index = index
    let mutable acc = acc
    try
        if index = (0 - 1) then
            __ret <- acc
            raise Return
        let node: Node = _idx nodes (index)
        let mutable res: int array = inorder (nodes) (node.left) (acc)
        res <- Array.append res [|node.data|]
        res <- inorder (nodes) (node.right) (res)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec size (nodes: Node array) (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nodes = nodes
    let mutable index = index
    try
        if index = (0 - 1) then
            __ret <- 0
            raise Return
        let node: Node = _idx nodes (index)
        __ret <- (1 + (size (nodes) (node.left))) + (size (nodes) (node.right))
        raise Return
        __ret
    with
        | Return -> __ret
let rec depth (nodes: Node array) (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable nodes = nodes
    let mutable index = index
    try
        if index = (0 - 1) then
            __ret <- 0
            raise Return
        let node: Node = _idx nodes (index)
        let left_depth: int = depth (nodes) (node.left)
        let right_depth: int = depth (nodes) (node.right)
        if left_depth > right_depth then
            __ret <- left_depth + 1
            raise Return
        __ret <- right_depth + 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_full (nodes: Node array) (index: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable nodes = nodes
    let mutable index = index
    try
        if index = (0 - 1) then
            __ret <- true
            raise Return
        let node: Node = _idx nodes (index)
        if ((node.left) = (0 - 1)) && ((node.right) = (0 - 1)) then
            __ret <- true
            raise Return
        if ((node.left) <> (0 - 1)) && ((node.right) <> (0 - 1)) then
            __ret <- (is_full (nodes) (node.left)) && (is_full (nodes) (node.right))
            raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec small_tree () =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    try
        let mutable arr: Node array = [||]
        arr <- Array.append arr [|{ data = 2; left = 1; right = 2 }|]
        arr <- Array.append arr [|{ data = 1; left = 0 - 1; right = 0 - 1 }|]
        arr <- Array.append arr [|{ data = 3; left = 0 - 1; right = 0 - 1 }|]
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec medium_tree () =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    try
        let mutable arr: Node array = [||]
        arr <- Array.append arr [|{ data = 4; left = 1; right = 4 }|]
        arr <- Array.append arr [|{ data = 2; left = 2; right = 3 }|]
        arr <- Array.append arr [|{ data = 1; left = 0 - 1; right = 0 - 1 }|]
        arr <- Array.append arr [|{ data = 3; left = 0 - 1; right = 0 - 1 }|]
        arr <- Array.append arr [|{ data = 5; left = 0 - 1; right = 5 }|]
        arr <- Array.append arr [|{ data = 6; left = 0 - 1; right = 6 }|]
        arr <- Array.append arr [|{ data = 7; left = 0 - 1; right = 0 - 1 }|]
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let small: Node array = small_tree()
printfn "%d" (size (small) (0))
printfn "%s" (_repr (inorder (small) (0) (Array.empty<int>)))
printfn "%d" (depth (small) (0))
printfn "%b" (is_full (small) (0))
let medium: Node array = medium_tree()
printfn "%d" (size (medium) (0))
printfn "%s" (_repr (inorder (medium) (0) (Array.empty<int>)))
printfn "%d" (depth (medium) (0))
printfn "%b" (is_full (medium) (0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
