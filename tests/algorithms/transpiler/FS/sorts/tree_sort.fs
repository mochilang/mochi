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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Node = {
    mutable _value: int
    mutable _left: int
    mutable _right: int
}
type TreeState = {
    mutable _nodes: Node array
    mutable _root: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_node (state: TreeState) (_value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable state = state
    let mutable _value = _value
    try
        state._nodes <- Array.append (state._nodes) [|{ _value = _value; _left = -1; _right = -1 }|]
        __ret <- (Seq.length (state._nodes)) - 1
        raise Return
        __ret
    with
        | Return -> __ret
and insert (state: TreeState) (_value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable state = state
    let mutable _value = _value
    try
        if (state._root) = (-1) then
            state._root <- new_node (state) (_value)
            __ret <- ()
            raise Return
        let mutable current: int = state._root
        let mutable _nodes: Node array = state._nodes
        while true do
            let mutable node: Node = _idx _nodes (int current)
            if _value < (node._value) then
                if (node._left) = (-1) then
                    let idx: int = new_node (state) (_value)
                    _nodes <- state._nodes
                    node._left <- idx
                    _nodes.[int current] <- node
                    state._nodes <- _nodes
                    __ret <- ()
                    raise Return
                current <- node._left
            else
                if _value > (node._value) then
                    if (node._right) = (-1) then
                        let idx: int = new_node (state) (_value)
                        _nodes <- state._nodes
                        node._right <- idx
                        _nodes.[int current] <- node
                        state._nodes <- _nodes
                        __ret <- ()
                        raise Return
                    current <- node._right
                else
                    __ret <- ()
                    raise Return
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
        let node: Node = _idx (state._nodes) (int idx)
        let mutable result: int array = inorder (state) (node._left)
        result <- Array.append result [|(node._value)|]
        let right_part: int array = inorder (state) (node._right)
        let mutable i: int = 0
        while i < (Seq.length (right_part)) do
            result <- Array.append result [|(_idx right_part (int i))|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and tree_sort (arr: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    try
        let mutable state: TreeState = { _nodes = Array.empty<Node>; _root = -1 }
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            insert (state) (_idx arr (int i))
            i <- i + 1
        if (state._root) = (-1) then
            __ret <- Array.empty<int>
            raise Return
        __ret <- inorder (state) (state._root)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (tree_sort (Array.empty<int>)))
printfn "%s" (_str (tree_sort (unbox<int array> [|1|])))
printfn "%s" (_str (tree_sort (unbox<int array> [|1; 2|])))
printfn "%s" (_str (tree_sort (unbox<int array> [|5; 2; 7|])))
printfn "%s" (_str (tree_sort (unbox<int array> [|5; -4; 9; 2; 7|])))
printfn "%s" (_str (tree_sort (unbox<int array> [|5; 6; 1; -1; 4; 37; 2; 7|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
