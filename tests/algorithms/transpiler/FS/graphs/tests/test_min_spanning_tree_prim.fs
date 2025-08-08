// Generated 2025-08-08 16:03 +0700

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
type Neighbor = {
    mutable _node: int
    mutable _cost: int
}
type EdgePair = {
    mutable _u: int
    mutable _v: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec prims_algorithm (adjacency: System.Collections.Generic.IDictionary<int, Neighbor array>) =
    let mutable __ret : EdgePair array = Unchecked.defaultof<EdgePair array>
    let mutable adjacency = adjacency
    try
        let mutable visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        visited.[0] <- true
        let mutable mst: EdgePair array = [||]
        let mutable count: int = 1
        let mutable total: int = 0
        for k in adjacency.Keys do
            total <- total + 1
        while count < total do
            let mutable best_u: int = 0
            let mutable best_v: int = 0
            let mutable best_cost: int = 2147483647
            for u_str in adjacency.Keys do
                let _u: int = int (u_str)
                if _dictGet visited (_u) then
                    for n in (_dictGet adjacency (_u)).Keys do
                        if (not (_dictGet visited (n._node))) && ((n._cost) < best_cost) then
                            best_cost <- n._cost
                            best_u <- _u
                            best_v <- n._node
            visited.[best_v] <- true
            mst <- Array.append mst [|{ _u = best_u; _v = best_v }|]
            count <- count + 1
        __ret <- mst
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_prim_successful_result () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let edges: int array array = [|[|0; 1; 4|]; [|0; 7; 8|]; [|1; 2; 8|]; [|7; 8; 7|]; [|7; 6; 1|]; [|2; 8; 2|]; [|8; 6; 6|]; [|2; 3; 7|]; [|2; 5; 4|]; [|6; 5; 2|]; [|3; 5; 14|]; [|3; 4; 9|]; [|5; 4; 10|]; [|1; 7; 11|]|]
        let mutable adjacency: System.Collections.Generic.IDictionary<int, Neighbor array> = _dictCreate []
        for e in edges do
            let _u: int = _idx e (0)
            let _v: int = _idx e (1)
            let w: int = _idx e (2)
            if not (adjacency.ContainsKey(_u)) then
                adjacency.[_u] <- [||]
            if not (adjacency.ContainsKey(_v)) then
                adjacency.[_v] <- [||]
            adjacency.[_u] <- Array.append (_dictGet adjacency (_u)) [|{ _node = _v; _cost = w }|]
            adjacency.[_v] <- Array.append (_dictGet adjacency (_v)) [|{ _node = _u; _cost = w }|]
        let result: EdgePair array = prims_algorithm (adjacency)
        let mutable seen: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        for e in result do
            let key1: string = ((_str (e._u)) + ",") + (_str (e._v))
            let key2: string = ((_str (e._v)) + ",") + (_str (e._u))
            seen.[key1] <- true
            seen.[key2] <- true
        let expected: int array array = [|[|7; 6; 1|]; [|2; 8; 2|]; [|6; 5; 2|]; [|0; 1; 4|]; [|2; 5; 4|]; [|2; 3; 7|]; [|0; 7; 8|]; [|3; 4; 9|]|]
        for ans in expected do
            let key: string = ((_str (_idx ans (0))) + ",") + (_str (_idx ans (1)))
            if not (_dictGet seen ((string (key)))) then
                __ret <- false
                raise Return
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%b" (test_prim_successful_result())
printfn "%b" (true)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
