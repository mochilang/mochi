// Generated 2025-08-07 12:04 +0700

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
type TreeState = {
    nodes: Node array
    root: int
}
let rec new_node (state: TreeState) (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable state = state
    let mutable value = value
    try
        state <- { state with nodes = Array.append (state.nodes) [|{ data = value; left = -1; right = -1 }|] }
        __ret <- (Seq.length (state.nodes)) - 1
        raise Return
        __ret
    with
        | Return -> __ret
and insert (state: TreeState) (value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable state = state
    let mutable value = value
    try
        if (state.root) = (-1) then
            state <- { state with root = new_node (state) (value) }
            __ret <- ()
            raise Return
        let mutable current: int = state.root
        let mutable nodes: Node array = state.nodes
        while true do
            let mutable node: Node = _idx nodes (current)
            if value < (node.data) then
                if (node.left) = (-1) then
                    node <- { node with left = new_node (state) (value) }
                    nodes.[current] <- node
                    state <- { state with nodes = nodes }
                    __ret <- ()
                    raise Return
                current <- node.left
            else
                if (node.right) = (-1) then
                    node <- { node with right = new_node (state) (value) }
                    nodes.[current] <- node
                    state <- { state with nodes = nodes }
                    __ret <- ()
                    raise Return
                current <- node.right
        __ret
    with
        | Return -> __ret
and inorder (state: TreeState) (idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable state = state
    let mutable idx = idx
    try
        if idx = (-1) then
            __ret <- Array.empty<int>
            raise Return
        let node: Node = _idx (state.nodes) (idx)
        let mutable result: int array = inorder (state) (node.left)
        result <- Array.append result [|node.data|]
        let right_part: int array = inorder (state) (node.right)
        let mutable i: int = 0
        while i < (Seq.length (right_part)) do
            result <- Array.append result [|_idx right_part (i)|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and make_tree () =
    let mutable __ret : TreeState = Unchecked.defaultof<TreeState>
    try
        let mutable state: TreeState = { nodes = [||]; root = -1 }
        insert (state) (15)
        insert (state) (10)
        insert (state) (25)
        insert (state) (6)
        insert (state) (14)
        insert (state) (20)
        insert (state) (60)
        __ret <- state
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let state: TreeState = make_tree()
        printfn "%s" ("Printing values of binary search tree in Inorder Traversal.")
        printfn "%s" (_repr (inorder (state) (state.root)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
