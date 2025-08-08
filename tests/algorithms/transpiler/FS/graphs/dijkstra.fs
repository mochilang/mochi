// Generated 2025-08-08 15:37 +0700

exception Break
exception Continue

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
type NodeCost = {
    mutable _node: string
    mutable _cost: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let G: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>> = _dictCreate [("A", _dictCreate [("B", 2); ("C", 5)]); ("B", _dictCreate [("A", 2); ("D", 3); ("E", 1); ("F", 1)]); ("C", _dictCreate [("A", 5); ("F", 3)]); ("D", _dictCreate [("B", 3)]); ("E", _dictCreate [("B", 4); ("F", 3)]); ("F", _dictCreate [("C", 3); ("E", 3)])]
let mutable heap: NodeCost array = [|{ _node = "E"; _cost = 0 }|]
let mutable visited: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
let mutable result: int = -1
try
    while (Seq.length (heap)) > 0 do
        try
            let mutable best_idx: int = 0
            let mutable i: int = 1
            while i < (Seq.length (heap)) do
                if ((_idx heap (i))._cost) < ((_idx heap (best_idx))._cost) then
                    best_idx <- i
                i <- i + 1
            let best: NodeCost = _idx heap (best_idx)
            let mutable new_heap: NodeCost array = [||]
            let mutable j: int = 0
            while j < (Seq.length (heap)) do
                if j <> best_idx then
                    new_heap <- Array.append new_heap [|(_idx heap (j))|]
                j <- j + 1
            heap <- new_heap
            let u: string = best._node
            let _cost: int = best._cost
            if visited.ContainsKey(u) then
                raise Continue
            visited.[u] <- true
            if u = "C" then
                result <- _cost
                raise Break
            try
                for v in (_dictGet G (u)).Keys do
                    try
                        if visited.ContainsKey(v) then
                            raise Continue
                        let next_cost = _cost + (int (_dictGet (_dictGet G (u)) (v)))
                        heap <- Array.append heap [|{ _node = v; _cost = next_cost }|]
                    with
                    | Continue -> ()
                    | Break -> raise Break
            with
            | Break -> ()
            | Continue -> ()
        with
        | Continue -> ()
        | Break -> raise Break
with
| Break -> ()
| Continue -> ()
printfn "%d" (result)
let G2: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>> = _dictCreate [("B", _dictCreate [("C", 1)]); ("C", _dictCreate [("D", 1)]); ("D", _dictCreate [("F", 1)]); ("E", _dictCreate [("B", 1); ("F", 3)]); ("F", _dictCreate [])]
let mutable heap2: NodeCost array = [|{ _node = "E"; _cost = 0 }|]
let mutable visited2: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
let mutable result2: int = -1
try
    while (Seq.length (heap2)) > 0 do
        try
            let mutable best2_idx: int = 0
            let mutable i2: int = 1
            while i2 < (Seq.length (heap2)) do
                if ((_idx heap2 (i2))._cost) < ((_idx heap2 (best2_idx))._cost) then
                    best2_idx <- i2
                i2 <- i2 + 1
            let best2: NodeCost = _idx heap2 (best2_idx)
            let mutable new_heap2: NodeCost array = [||]
            let mutable j2: int = 0
            while j2 < (Seq.length (heap2)) do
                if j2 <> best2_idx then
                    new_heap2 <- Array.append new_heap2 [|(_idx heap2 (j2))|]
                j2 <- j2 + 1
            heap2 <- new_heap2
            let u2: string = best2._node
            let cost2: int = best2._cost
            if visited2.ContainsKey(u2) then
                raise Continue
            visited2.[u2] <- true
            if u2 = "F" then
                result2 <- cost2
                raise Break
            try
                for v2 in (_dictGet G2 (u2)).Keys do
                    try
                        if visited2.ContainsKey(v2) then
                            raise Continue
                        let next_cost2 = cost2 + (int (_dictGet (_dictGet G2 (u2)) (v2)))
                        heap2 <- Array.append heap2 [|{ _node = v2; _cost = next_cost2 }|]
                    with
                    | Continue -> ()
                    | Break -> raise Break
            with
            | Break -> ()
            | Continue -> ()
        with
        | Continue -> ()
        | Break -> raise Break
with
| Break -> ()
| Continue -> ()
printfn "%d" (result2)
let G3: System.Collections.Generic.IDictionary<string, System.Collections.Generic.IDictionary<string, int>> = _dictCreate [("B", _dictCreate [("C", 1)]); ("C", _dictCreate [("D", 1)]); ("D", _dictCreate [("F", 1)]); ("E", _dictCreate [("B", 1); ("G", 2)]); ("F", _dictCreate []); ("G", _dictCreate [("F", 1)])]
let mutable heap3: NodeCost array = [|{ _node = "E"; _cost = 0 }|]
let mutable visited3: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
let mutable result3: int = -1
try
    while (Seq.length (heap3)) > 0 do
        try
            let mutable best3_idx: int = 0
            let mutable i3: int = 1
            while i3 < (Seq.length (heap3)) do
                if ((_idx heap3 (i3))._cost) < ((_idx heap3 (best3_idx))._cost) then
                    best3_idx <- i3
                i3 <- i3 + 1
            let best3: NodeCost = _idx heap3 (best3_idx)
            let mutable new_heap3: NodeCost array = [||]
            let mutable j3: int = 0
            while j3 < (Seq.length (heap3)) do
                if j3 <> best3_idx then
                    new_heap3 <- Array.append new_heap3 [|(_idx heap3 (j3))|]
                j3 <- j3 + 1
            heap3 <- new_heap3
            let u3: string = best3._node
            let cost3: int = best3._cost
            if visited3.ContainsKey(u3) then
                raise Continue
            visited3.[u3] <- true
            if u3 = "F" then
                result3 <- cost3
                raise Break
            try
                for v3 in (_dictGet G3 (u3)).Keys do
                    try
                        if visited3.ContainsKey(v3) then
                            raise Continue
                        let next_cost3 = cost3 + (int (_dictGet (_dictGet G3 (u3)) (v3)))
                        heap3 <- Array.append heap3 [|{ _node = v3; _cost = next_cost3 }|]
                    with
                    | Continue -> ()
                    | Break -> raise Break
            with
            | Break -> ()
            | Continue -> ()
        with
        | Continue -> ()
        | Break -> raise Break
with
| Break -> ()
| Continue -> ()
printfn "%d" (result3)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
