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
type Node = {
    mutable _data: int
    mutable _next: int
}
let rec has_loop (nodes: Node array) (head: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable nodes = nodes
    let mutable head = head
    try
        let mutable slow: int = head
        let mutable fast: int = head
        while fast <> (0 - 1) do
            let fast_node1: Node = _idx nodes (fast)
            if (fast_node1._next) = (0 - 1) then
                __ret <- false
                raise Return
            let fast_node2: Node = _idx nodes (fast_node1._next)
            if (fast_node2._next) = (0 - 1) then
                __ret <- false
                raise Return
            let slow_node: Node = _idx nodes (slow)
            slow <- slow_node._next
            fast <- fast_node2._next
            if slow = fast then
                __ret <- true
                raise Return
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and make_nodes (values: int array) =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    let mutable values = values
    try
        let mutable nodes: Node array = [||]
        let mutable i: int = 0
        while i < (Seq.length (values)) do
            let next_idx: int = if i = ((Seq.length (values)) - 1) then (0 - 1) else (i + 1)
            nodes <- Array.append nodes [|{ _data = _idx values (i); _next = next_idx }|]
            i <- i + 1
        __ret <- nodes
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable list1: Node array = make_nodes (unbox<int array> [|1; 2; 3; 4|])
        printfn "%s" (_str (has_loop (list1) (0)))
        list1.[3]._next <- 1
        printfn "%s" (_str (has_loop (list1) (0)))
        let list2: Node array = make_nodes (unbox<int array> [|5; 6; 5; 6|])
        printfn "%s" (_str (has_loop (list2) (0)))
        let list3: Node array = make_nodes (unbox<int array> [|1|])
        printfn "%s" (_str (has_loop (list3) (0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
