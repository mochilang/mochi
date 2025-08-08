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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type KDNode = {
    mutable _point: float array
    mutable _left: int
    mutable _right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_kd_node (_point: float array) (_left: int) (_right: int) =
    let mutable __ret : KDNode = Unchecked.defaultof<KDNode>
    let mutable _point = _point
    let mutable _left = _left
    let mutable _right = _right
    try
        __ret <- { _point = _point; _left = _left; _right = _right }
        raise Return
        __ret
    with
        | Return -> __ret
let mutable nodes: KDNode array = [||]
nodes <- Array.append nodes [|(make_kd_node (unbox<float array> [|2.0; 3.0|]) (1) (2))|]
nodes <- Array.append nodes [|(make_kd_node (unbox<float array> [|1.0; 5.0|]) (-1) (-1))|]
nodes <- Array.append nodes [|(make_kd_node (unbox<float array> [|4.0; 2.0|]) (-1) (-1))|]
let root: KDNode = _idx nodes (0)
let left_child: KDNode = _idx nodes (1)
let right_child: KDNode = _idx nodes (2)
printfn "%s" (_str (root._point))
printfn "%s" (_str (root._left))
printfn "%s" (_str (root._right))
printfn "%s" (_str (left_child._point))
printfn "%s" (_str (right_child._point))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
