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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
type Node = {
    children: System.Collections.Generic.IDictionary<string, int>
    is_leaf: bool
}
type Trie = {
    nodes: Node array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec new_trie () =
    let mutable __ret : Trie = Unchecked.defaultof<Trie>
    try
        __ret <- { nodes = [|{ children = _dictCreate []; is_leaf = false }|] }
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_key (m: System.Collections.Generic.IDictionary<string, int>) (k: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, int> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, int>>
    let mutable m = m
    let mutable k = k
    try
        let mutable out: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        for key in m.Keys do
            if key <> k then
                out.[key] <- m.[(string (key))]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec insert (trie: Trie) (word: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable trie = trie
    let mutable word = word
    try
        let mutable nodes: Node array = trie.nodes
        let mutable curr: int = 0
        let mutable i: int = 0
        while i < (String.length (word)) do
            let ch: string = string (word.[i])
            let mutable child_idx: int = -1
            let children: System.Collections.Generic.IDictionary<string, int> = (_idx nodes (curr)).children
            if children.ContainsKey(ch) then
                child_idx <- children.[(string (ch))]
            else
                let new_node: Node = { children = _dictCreate []; is_leaf = false }
                nodes <- Array.append nodes [|new_node|]
                child_idx <- (Seq.length (nodes)) - 1
                let mutable new_children: System.Collections.Generic.IDictionary<string, int> = children
                new_children.[ch] <- child_idx
                let mutable node: Node = _idx nodes (curr)
                node <- { node with children = new_children }
                nodes.[curr] <- node
            curr <- child_idx
            i <- i + 1
        let mutable node: Node = _idx nodes (curr)
        node <- { node with is_leaf = true }
        nodes.[curr] <- node
        trie <- { trie with nodes = nodes }
        __ret
    with
        | Return -> __ret
let rec insert_many (trie: Trie) (words: string array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable trie = trie
    let mutable words = words
    try
        for w in Seq.map string (words) do
            insert (trie) (w)
        __ret
    with
        | Return -> __ret
let rec find (trie: Trie) (word: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable trie = trie
    let mutable word = word
    try
        let mutable nodes: Node array = trie.nodes
        let mutable curr: int = 0
        let mutable i: int = 0
        while i < (String.length (word)) do
            let ch: string = string (word.[i])
            let children: System.Collections.Generic.IDictionary<string, int> = (_idx nodes (curr)).children
            if not (children.ContainsKey(ch)) then
                __ret <- false
                raise Return
            curr <- children.[(string (ch))]
            i <- i + 1
        let mutable node: Node = _idx nodes (curr)
        __ret <- node.is_leaf
        raise Return
        __ret
    with
        | Return -> __ret
let rec delete (trie: Trie) (word: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable trie = trie
    let mutable word = word
    try
        let mutable nodes: Node array = trie.nodes
        let rec _delete (idx: int) (pos: int) =
            let mutable __ret : bool = Unchecked.defaultof<bool>
            let mutable idx = idx
            let mutable pos = pos
            try
                if pos = (String.length (word)) then
                    let mutable node: Node = _idx nodes (idx)
                    if (node.is_leaf) = false then
                        __ret <- false
                        raise Return
                    node <- { node with is_leaf = false }
                    nodes.[idx] <- node
                    __ret <- (Seq.length (node.children)) = 0
                    raise Return
                let mutable node: Node = _idx nodes (idx)
                let children: System.Collections.Generic.IDictionary<string, int> = node.children
                let ch: string = string (word.[pos])
                if not (children.ContainsKey(ch)) then
                    __ret <- false
                    raise Return
                let mutable child_idx: int = children.[(string (ch))]
                let should_delete: bool = _delete (child_idx) (pos + 1)
                node <- _idx nodes (idx)
                if should_delete then
                    let mutable new_children: System.Collections.Generic.IDictionary<string, int> = remove_key (node.children) (ch)
                    node <- { node with children = new_children }
                    nodes.[idx] <- node
                    __ret <- ((Seq.length (new_children)) = 0) && ((node.is_leaf) = false)
                    raise Return
                nodes.[idx] <- node
                __ret <- false
                raise Return
                __ret
            with
                | Return -> __ret
        _delete (0) (0)
        trie <- { trie with nodes = nodes }
        __ret
    with
        | Return -> __ret
let rec print_words (trie: Trie) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable trie = trie
    try
        let rec dfs (idx: int) (word: string) =
            let mutable __ret = ()
            let mutable idx = idx
            let mutable word = word
            try
                let mutable node: Node = _idx (trie.nodes) (idx)
                if node.is_leaf then
                    printfn "%s" (word)
                for key in (node.children).Keys do
                    dfs (node.children.[(string (key))]) (word + key)
                __ret
            with
                | Return -> __ret
        dfs (0) ("")
        __ret
    with
        | Return -> __ret
let rec test_trie () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let words: string array = [|"banana"; "bananas"; "bandana"; "band"; "apple"; "all"; "beast"|]
        let trie: Trie = new_trie()
        insert_many (trie) (words)
        let mutable ok: bool = true
        for w in Seq.map string (words) do
            ok <- ok && (find (trie) (w))
        ok <- ok && (find (trie) ("banana"))
        let mutable t: bool = find (trie) ("bandanas")
        ok <- ok && (t = false)
        let mutable t2: bool = find (trie) ("apps")
        ok <- ok && (t2 = false)
        ok <- ok && (find (trie) ("apple"))
        ok <- ok && (find (trie) ("all"))
        delete (trie) ("all")
        let mutable t3: bool = find (trie) ("all")
        ok <- ok && (t3 = false)
        delete (trie) ("banana")
        let mutable t4: bool = find (trie) ("banana")
        ok <- ok && (t4 = false)
        ok <- ok && (find (trie) ("bananas"))
        __ret <- ok
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_results (msg: string) (passes: bool) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    let mutable passes = passes
    try
        if passes then
            printfn "%s" (msg + " works!")
        else
            printfn "%s" (msg + " doesn't work :(")
        __ret
    with
        | Return -> __ret
let trie: Trie = new_trie()
print_results ("Testing trie functionality") (test_trie())
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
