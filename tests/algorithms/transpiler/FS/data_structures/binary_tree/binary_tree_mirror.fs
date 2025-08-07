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
open System.Collections.Generic

let rec binary_tree_mirror_dict (tree: System.Collections.Generic.IDictionary<int, int array>) (root: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable tree = tree
    let mutable root = root
    try
        if (root = 0) || (not (tree.ContainsKey(root))) then
            __ret <- ()
            raise Return
        let children: int array = tree.[root]
        let left: int = _idx children (0)
        let right: int = _idx children (1)
        tree.[root] <- [|right; left|]
        binary_tree_mirror_dict (tree) (left)
        binary_tree_mirror_dict (tree) (right)
        __ret
    with
        | Return -> __ret
and binary_tree_mirror (binary_tree: System.Collections.Generic.IDictionary<int, int array>) (root: int) =
    let mutable __ret : System.Collections.Generic.IDictionary<int, int array> = Unchecked.defaultof<System.Collections.Generic.IDictionary<int, int array>>
    let mutable binary_tree = binary_tree
    let mutable root = root
    try
        if (Seq.length (binary_tree)) = 0 then
            failwith ("binary tree cannot be empty")
        if not (binary_tree.ContainsKey(root)) then
            failwith (("root " + (_str (root))) + " is not present in the binary_tree")
        let mutable tree_copy: System.Collections.Generic.IDictionary<int, int array> = _dictCreate []
        for k in binary_tree.Keys do
            tree_copy.[k] <- binary_tree.[k]
        binary_tree_mirror_dict (tree_copy) (root)
        __ret <- tree_copy
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let binary_tree: System.Collections.Generic.IDictionary<int, int array> = _dictCreate [(1, [|2; 3|]); (2, [|4; 5|]); (3, [|6; 7|]); (7, [|8; 9|])]
        printfn "%s" ("Binary tree: " + (_str (binary_tree)))
        let mirrored: System.Collections.Generic.IDictionary<int, int array> = binary_tree_mirror (binary_tree) (1)
        printfn "%s" ("Binary tree mirror: " + (_str (mirrored)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
