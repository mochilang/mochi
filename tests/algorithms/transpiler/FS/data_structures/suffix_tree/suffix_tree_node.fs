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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type SuffixTreeNode = {
    children: System.Collections.Generic.IDictionary<string, int>
    is_end_of_string: bool
    start: int
    ``end``: int
    suffix_link: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec new_suffix_tree_node (children: System.Collections.Generic.IDictionary<string, int>) (is_end_of_string: bool) (start: int) (``end``: int) (suffix_link: int) =
    let mutable __ret : SuffixTreeNode = Unchecked.defaultof<SuffixTreeNode>
    let mutable children = children
    let mutable is_end_of_string = is_end_of_string
    let mutable start = start
    let mutable ``end`` = ``end``
    let mutable suffix_link = suffix_link
    try
        __ret <- { children = children; is_end_of_string = is_end_of_string; start = start; ``end`` = ``end``; suffix_link = suffix_link }
        raise Return
        __ret
    with
        | Return -> __ret
let rec empty_suffix_tree_node () =
    let mutable __ret : SuffixTreeNode = Unchecked.defaultof<SuffixTreeNode>
    try
        __ret <- new_suffix_tree_node (_dictCreate []) (false) (0 - 1) (0 - 1) (0 - 1)
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
let root: SuffixTreeNode = new_suffix_tree_node (_dictCreate [("a", 1)]) (false) (0 - 1) (0 - 1) (0 - 1)
let leaf: SuffixTreeNode = new_suffix_tree_node (_dictCreate []) (true) (0) (2) (0)
let mutable nodes: SuffixTreeNode array = [|root; leaf|]
let mutable root_check: SuffixTreeNode = _idx nodes (0)
let mutable leaf_check: SuffixTreeNode = _idx nodes (1)
printfn "%s" (_str (has_key (root_check.children) ("a")))
printfn "%s" (_str (leaf_check.is_end_of_string))
printfn "%s" (_str (leaf_check.start))
printfn "%s" (_str (leaf_check.``end``))
printfn "%s" (_str (leaf_check.suffix_link))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
