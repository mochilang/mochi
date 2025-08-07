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
type Node = {
    value: int
    left: int
    right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec node_sum (tree: Node array) (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable tree = tree
    let mutable index = index
    try
        if index = (-1) then
            __ret <- 0
            raise Return
        let node: Node = _idx tree (index)
        __ret <- ((node.value) + (node_sum (tree) (node.left))) + (node_sum (tree) (node.right))
        raise Return
        __ret
    with
        | Return -> __ret
let example: Node array = [|{ value = 10; left = 1; right = 2 }; { value = 5; left = 3; right = -1 }; { value = -3; left = 4; right = 5 }; { value = 12; left = -1; right = -1 }; { value = 8; left = -1; right = -1 }; { value = 0; left = -1; right = -1 }|]
printfn "%d" (node_sum (example) (0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
