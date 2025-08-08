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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type KDNode = {
    mutable _point: float array
    mutable _left: int
    mutable _right: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let mutable tree: KDNode array = [||]
let rec sort_points (points: float array array) (axis: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable points = points
    let mutable axis = axis
    try
        let mutable arr: float array array = points
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let mutable j: int = 0
            while j < ((Seq.length (arr)) - 1) do
                if (_idx (_idx arr (j)) (axis)) > (_idx (_idx arr (j + 1)) (axis)) then
                    let tmp: float array = _idx arr (j)
                    arr.[j] <- _idx arr (j + 1)
                    arr.[j + 1] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_kdtree (points: float array array) (depth: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable points = points
    let mutable depth = depth
    try
        if (Seq.length (points)) = 0 then
            __ret <- 0 - 1
            raise Return
        let k: int = Seq.length (_idx points (0))
        let axis: int = ((depth % k + k) % k)
        let sorted: float array array = sort_points (points) (axis)
        let median_idx: int = _floordiv (Seq.length (sorted)) 2
        let left_points: float array array = Array.sub sorted 0 (median_idx - 0)
        let right_points: float array array = Array.sub sorted (median_idx + 1) ((Seq.length (sorted)) - (median_idx + 1))
        let mutable idx: int = Seq.length (tree)
        tree <- Array.append tree [|{ _point = _idx sorted (median_idx); _left = 0 - 1; _right = 0 - 1 }|]
        let left_idx: int = build_kdtree (left_points) (depth + 1)
        let right_idx: int = build_kdtree (right_points) (depth + 1)
        let mutable node: KDNode = _idx tree (idx)
        node._left <- left_idx
        node._right <- right_idx
        tree <- _arrset tree (idx) (node)
        __ret <- idx
        raise Return
        __ret
    with
        | Return -> __ret
let pts: float array array = [|[|2.0; 3.0|]; [|5.0; 4.0|]; [|9.0; 6.0|]; [|4.0; 7.0|]; [|8.0; 1.0|]; [|7.0; 2.0|]|]
let root: int = build_kdtree (pts) (0)
printfn "%s" (_str (tree))
printfn "%d" (root)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
