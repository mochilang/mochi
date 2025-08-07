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
    data: float array
    left: int array
    right: int array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let NONE: int = 0 - 1
let rec inorder (tree: Tree) (index: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable tree = tree
    let mutable index = index
    try
        let mutable res: float array = [||]
        if index = NONE then
            __ret <- res
            raise Return
        let left_idx: int = _idx (tree.left) (index)
        if left_idx <> NONE then
            res <- unbox<float array> (Array.append (res) (inorder (tree) (left_idx)))
        res <- Array.append res [|_idx (tree.data) (index)|]
        let right_idx: int = _idx (tree.right) (index)
        if right_idx <> NONE then
            res <- unbox<float array> (Array.append (res) (inorder (tree) (right_idx)))
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_sorted (tree: Tree) (index: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable tree = tree
    let mutable index = index
    try
        if index = NONE then
            __ret <- true
            raise Return
        let left_idx: int = _idx (tree.left) (index)
        if left_idx <> NONE then
            if (_idx (tree.data) (index)) < (_idx (tree.data) (left_idx)) then
                __ret <- false
                raise Return
            if not (is_sorted (tree) (left_idx)) then
                __ret <- false
                raise Return
        let right_idx: int = _idx (tree.right) (index)
        if right_idx <> NONE then
            if (_idx (tree.data) (index)) > (_idx (tree.data) (right_idx)) then
                __ret <- false
                raise Return
            if not (is_sorted (tree) (right_idx)) then
                __ret <- false
                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let tree1: Tree = { data = [|2.1; 2.0; 2.2|]; left = [|1; NONE; NONE|]; right = [|2; NONE; NONE|] }
printfn "%s" ((("Tree " + (_str (inorder (tree1) (0)))) + " is sorted: ") + (_str (is_sorted (tree1) (0))))
let tree2: Tree = { data = [|2.1; 2.0; 2.0|]; left = [|1; NONE; NONE|]; right = [|2; NONE; NONE|] }
printfn "%s" ((("Tree " + (_str (inorder (tree2) (0)))) + " is sorted: ") + (_str (is_sorted (tree2) (0))))
let tree3: Tree = { data = [|2.1; 2.0; 2.1|]; left = [|1; NONE; NONE|]; right = [|2; NONE; NONE|] }
printfn "%s" ((("Tree " + (_str (inorder (tree3) (0)))) + " is sorted: ") + (_str (is_sorted (tree3) (0))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
