// Generated 2025-08-08 11:10 +0700

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

let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Node = {
    mutable _children: System.Collections.Generic.IDictionary<string, int>
    mutable _is_end_of_string: bool
    mutable _start: int
    mutable ``end``: int
}
type SuffixTree = {
    mutable _text: string
    mutable _nodes: Node array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_node () =
    let mutable __ret : Node = Unchecked.defaultof<Node>
    try
        __ret <- { _children = _dictCreate []; _is_end_of_string = false; _start = -1; ``end`` = -1 }
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
        let mutable _nodes: Node array = tree._nodes
        let mutable node_idx: int = 0
        let mutable j: int = 0
        while j < (String.length (suffix)) do
            let ch: string = _substring suffix j (j + 1)
            let mutable node: Node = _idx _nodes (node_idx)
            let mutable _children: System.Collections.Generic.IDictionary<string, int> = node._children
            if not (has_key (_children) (ch)) then
                _nodes <- Array.append _nodes [|(new_node())|]
                let new_idx: int = (Seq.length (_nodes)) - 1
                _children.[ch] <- new_idx
            node._children <- _children
            _nodes.[node_idx] <- node
            node_idx <- _dictGet _children ((string (ch)))
            j <- j + 1
        let mutable node: Node = _idx _nodes (node_idx)
        node._is_end_of_string <- true
        node._start <- index
        node.``end`` <- (index + (String.length (suffix))) - 1
        _nodes.[node_idx] <- node
        tree._nodes <- _nodes
        __ret <- tree
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_suffix_tree (tree: SuffixTree) =
    let mutable __ret : SuffixTree = Unchecked.defaultof<SuffixTree>
    let mutable tree = tree
    try
        let _text: string = tree._text
        let n: int = String.length (_text)
        let mutable i: int = 0
        let mutable t: SuffixTree = tree
        while i < n do
            let mutable suffix: string = ""
            let mutable k: int = i
            while k < n do
                suffix <- suffix + (_substring _text k (k + 1))
                k <- k + 1
            t <- add_suffix (t) (suffix) (i)
            i <- i + 1
        __ret <- t
        raise Return
        __ret
    with
        | Return -> __ret
let rec new_suffix_tree (_text: string) =
    let mutable __ret : SuffixTree = Unchecked.defaultof<SuffixTree>
    let mutable _text = _text
    try
        let mutable tree: SuffixTree = { _text = _text; _nodes = [||] }
        tree._nodes <- Array.append (tree._nodes) [|(new_node())|]
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
        let mutable _nodes: Node array = tree._nodes
        while i < (String.length (pattern)) do
            let ch: string = _substring pattern i (i + 1)
            let node: Node = _idx _nodes (node_idx)
            let _children: System.Collections.Generic.IDictionary<string, int> = node._children
            if not (has_key (_children) (ch)) then
                __ret <- false
                raise Return
            node_idx <- _dictGet _children ((string (ch)))
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
