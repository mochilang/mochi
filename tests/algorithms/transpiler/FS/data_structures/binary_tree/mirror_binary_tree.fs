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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Tree = {
    values: int array
    left: int array
    right: int array
    root: int
}
let rec mirror_node (left: int array) (right: int array) (idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable left = left
    let mutable right = right
    let mutable idx = idx
    try
        if idx = (-1) then
            __ret <- ()
            raise Return
        let temp: int = _idx left (idx)
        left.[idx] <- _idx right (idx)
        right.[idx] <- temp
        mirror_node (left) (right) (_idx left (idx))
        mirror_node (left) (right) (_idx right (idx))
        __ret
    with
        | Return -> __ret
and mirror (tree: Tree) =
    let mutable __ret : Tree = Unchecked.defaultof<Tree>
    let mutable tree = tree
    try
        mirror_node (tree.left) (tree.right) (tree.root)
        __ret <- tree
        raise Return
        __ret
    with
        | Return -> __ret
and inorder (tree: Tree) (idx: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable tree = tree
    let mutable idx = idx
    try
        if idx = (-1) then
            __ret <- Array.empty<int>
            raise Return
        let left_vals: int array = inorder (tree) (_idx (tree.left) (idx))
        let right_vals: int array = inorder (tree) (_idx (tree.right) (idx))
        __ret <- unbox<int array> (Array.append (Array.append (left_vals) ([|_idx (tree.values) (idx)|])) (right_vals))
        raise Return
        __ret
    with
        | Return -> __ret
and make_tree_zero () =
    let mutable __ret : Tree = Unchecked.defaultof<Tree>
    try
        __ret <- { values = [|0|]; left = [|-1|]; right = [|-1|]; root = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and make_tree_seven () =
    let mutable __ret : Tree = Unchecked.defaultof<Tree>
    try
        __ret <- { values = [|1; 2; 3; 4; 5; 6; 7|]; left = [|1; 3; 5; -1; -1; -1; -1|]; right = [|2; 4; 6; -1; -1; -1; -1|]; root = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and make_tree_nine () =
    let mutable __ret : Tree = Unchecked.defaultof<Tree>
    try
        __ret <- { values = [|1; 2; 3; 4; 5; 6; 7; 8; 9|]; left = [|1; 3; -1; 6; -1; -1; -1; -1; -1|]; right = [|2; 4; 5; 7; 8; -1; -1; -1; -1|]; root = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let names: string array = [|"zero"; "seven"; "nine"|]
        let trees: Tree array = [|make_tree_zero(); make_tree_seven(); make_tree_nine()|]
        let mutable i: int = 0
        while i < (Seq.length (trees)) do
            let tree: Tree = _idx trees (i)
            printfn "%s" ((("      The " + (_idx names (i))) + " tree: ") + (_str (inorder (tree) (tree.root))))
            let mirrored: Tree = mirror (tree)
            printfn "%s" ((("Mirror of " + (_idx names (i))) + " tree: ") + (_str (inorder (mirrored) (mirrored.root))))
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
