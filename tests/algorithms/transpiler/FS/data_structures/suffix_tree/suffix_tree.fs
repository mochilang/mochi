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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

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
    children: System.Collections.Generic.IDictionary<string, int>
    is_end_of_string: bool
    start: int
    ``end``: int
}
type SuffixTree = {
    text: string
    nodes: Node array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_node () =
    let mutable __ret : Node = Unchecked.defaultof<Node>
    try
        __ret <- { children = _dictCreate []; is_end_of_string = false; start = -1; ``end`` = -1 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec has_key (m: System.Collections.Generic.IDictionary<string, int>) (k: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable m = m
    let mutable k = k
    try
        for key in m.Keys do
            if key = k then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec add_suffix (tree: SuffixTree) (suffix: string) (index: int) =
    let mutable __ret : SuffixTree = Unchecked.defaultof<SuffixTree>
    let mutable tree = tree
    let mutable suffix = suffix
    let mutable index = index
    try
        let mutable nodes: Node array = tree.nodes
        let mutable node_idx: int = 0
        let mutable j: int = 0
        while j < (String.length (suffix)) do
            let ch: string = _substring suffix j (j + 1)
            let mutable node: Node = _idx nodes (node_idx)
            let mutable children: System.Collections.Generic.IDictionary<string, int> = node.children
            if not (has_key (children) (ch)) then
                nodes <- Array.append nodes [|new_node()|]
                let new_idx: int = (Seq.length (nodes)) - 1
                children.[ch] <- new_idx
            node <- { node with children = children }
            nodes.[node_idx] <- node
            node_idx <- children.[(string (ch))]
            j <- j + 1
        let mutable node: Node = _idx nodes (node_idx)
        node <- { node with is_end_of_string = true }
        node <- { node with start = index }
        node <- { node with ``end`` = (index + (String.length (suffix))) - 1 }
        nodes.[node_idx] <- node
        tree <- { tree with nodes = nodes }
        __ret <- tree
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_suffix_tree (tree: SuffixTree) =
    let mutable __ret : SuffixTree = Unchecked.defaultof<SuffixTree>
    let mutable tree = tree
    try
        let text: string = tree.text
        let n: int = String.length (text)
        let mutable i: int = 0
        let mutable t: SuffixTree = tree
        while i < n do
            let mutable suffix: string = ""
            let mutable k: int = i
            while k < n do
                suffix <- suffix + (_substring text k (k + 1))
                k <- k + 1
            t <- add_suffix (t) (suffix) (i)
            i <- i + 1
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
let rec new_suffix_tree (text: string) =
    let mutable __ret : SuffixTree = Unchecked.defaultof<SuffixTree>
    let mutable text = text
    try
        let mutable tree: SuffixTree = { text = text; nodes = [||] }
        tree <- { tree with nodes = Array.append (tree.nodes) [|new_node()|] }
        tree <- build_suffix_tree (tree)
        __ret <- tree
        raise Return
        __ret
    with
        | Return -> __ret
let rec search (tree: SuffixTree) (pattern: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable tree = tree
    let mutable pattern = pattern
    try
        let mutable node_idx: int = 0
        let mutable i: int = 0
        let mutable nodes: Node array = tree.nodes
        while i < (String.length (pattern)) do
            let ch: string = _substring pattern i (i + 1)
            let node: Node = _idx nodes (node_idx)
            let children: System.Collections.Generic.IDictionary<string, int> = node.children
            if not (has_key (children) (ch)) then
                __ret <- false
                raise Return
            node_idx <- children.[(string (ch))]
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let st: SuffixTree = new_suffix_tree ("bananas")
printfn "%s" (_str (search (st) ("ana")))
printfn "%s" (_str (search (st) ("apple")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
