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
type KDNode = {
    point: float array
    left: int
    right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_kd_node (point: float array) (left: int) (right: int) =
    let mutable __ret : KDNode = Unchecked.defaultof<KDNode>
    let mutable point = point
    let mutable left = left
    let mutable right = right
    try
        __ret <- { point = point; left = left; right = right }
        raise Return
        __ret
    with
        | Return -> __ret
let mutable nodes: KDNode array = [||]
nodes <- Array.append nodes [|make_kd_node (unbox<float array> [|2.0; 3.0|]) (1) (2)|]
nodes <- Array.append nodes [|make_kd_node (unbox<float array> [|1.0; 5.0|]) (-1) (-1)|]
nodes <- Array.append nodes [|make_kd_node (unbox<float array> [|4.0; 2.0|]) (-1) (-1)|]
let root: KDNode = _idx nodes (0)
let left_child: KDNode = _idx nodes (1)
let right_child: KDNode = _idx nodes (2)
printfn "%s" (_str (root.point))
printfn "%s" (_str (root.left))
printfn "%s" (_str (root.right))
printfn "%s" (_str (left_child.point))
printfn "%s" (_str (right_child.point))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
