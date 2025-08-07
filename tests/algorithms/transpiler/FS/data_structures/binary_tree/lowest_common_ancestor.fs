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

let rec pow2 (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable exp = exp
    try
        let mutable res: int = 1
        let mutable i: int = 0
        while i < exp do
            res <- res * 2
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and create_sparse (max_node: int) (parent: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable max_node = max_node
    let mutable parent = parent
    try
        let mutable j: int = 1
        while (pow2 (j)) < max_node do
            let mutable i: int = 1
            while i <= max_node do
                parent.[j].[i] <- _idx (_idx parent (j - 1)) (_idx (_idx parent (j - 1)) (i))
                i <- i + 1
            j <- j + 1
        __ret <- parent
        raise Return
        __ret
    with
        | Return -> __ret
and lowest_common_ancestor (u: int) (v: int) (level: int array) (parent: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable u = u
    let mutable v = v
    let mutable level = level
    let mutable parent = parent
    try
        if (_idx level (u)) < (_idx level (v)) then
            let temp: int = u
            u <- v
            v <- temp
        let mutable i: int = 18
        while i >= 0 do
            if ((_idx level (u)) - (pow2 (i))) >= (_idx level (v)) then
                u <- _idx (_idx parent (i)) (u)
            i <- i - 1
        if u = v then
            __ret <- u
            raise Return
        i <- 18
        while i >= 0 do
            let pu: int = _idx (_idx parent (i)) (u)
            let pv: int = _idx (_idx parent (i)) (v)
            if (pu <> 0) && (pu <> pv) then
                u <- pu
                v <- pv
            i <- i - 1
        __ret <- _idx (_idx parent (0)) (u)
        raise Return
        __ret
    with
        | Return -> __ret
and breadth_first_search (level: int array) (parent: int array array) (max_node: int) (graph: System.Collections.Generic.IDictionary<int, int array>) (root: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable level = level
    let mutable parent = parent
    let mutable max_node = max_node
    let mutable graph = graph
    let mutable root = root
    try
        level.[root] <- 0
        let mutable q: int array = [||]
        q <- Array.append q [|root|]
        let mutable head: int = 0
        while head < (Seq.length (q)) do
            let mutable u: int = _idx q (head)
            head <- head + 1
            let adj: int array = graph.[u]
            let mutable j: int = 0
            while j < (Seq.length (adj)) do
                let mutable v: int = _idx adj (j)
                if (_idx level (v)) = (0 - 1) then
                    level.[v] <- (_idx level (u)) + 1
                    parent.[0].[v] <- u
                    q <- Array.append q [|v|]
                j <- j + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let max_node: int = 13
        let mutable parent: int array array = [||]
        let mutable i: int = 0
        while i < 20 do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (max_node + 10) do
                row <- Array.append row [|0|]
                j <- j + 1
            parent <- Array.append parent [|row|]
            i <- i + 1
        let mutable level: int array = [||]
        i <- 0
        while i < (max_node + 10) do
            level <- Array.append level [|0 - 1|]
            i <- i + 1
        let mutable graph: System.Collections.Generic.IDictionary<int, int array> = _dictCreate []
        graph.[1] <- [|2; 3; 4|]
        graph.[2] <- [|5|]
        graph.[3] <- [|6; 7|]
        graph.[4] <- [|8|]
        graph.[5] <- [|9; 10|]
        graph.[6] <- [|11|]
        graph.[7] <- [||]
        graph.[8] <- [|12; 13|]
        graph.[9] <- [||]
        graph.[10] <- [||]
        graph.[11] <- [||]
        graph.[12] <- [||]
        graph.[13] <- [||]
        breadth_first_search (level) (parent) (max_node) (graph) (1)
        parent <- create_sparse (max_node) (parent)
        printfn "%s" ("LCA of node 1 and 3 is: " + (_str (lowest_common_ancestor (1) (3) (level) (parent))))
        printfn "%s" ("LCA of node 5 and 6 is: " + (_str (lowest_common_ancestor (5) (6) (level) (parent))))
        printfn "%s" ("LCA of node 7 and 11 is: " + (_str (lowest_common_ancestor (7) (11) (level) (parent))))
        printfn "%s" ("LCA of node 6 and 7 is: " + (_str (lowest_common_ancestor (6) (7) (level) (parent))))
        printfn "%s" ("LCA of node 4 and 12 is: " + (_str (lowest_common_ancestor (4) (12) (level) (parent))))
        printfn "%s" ("LCA of node 8 and 8 is: " + (_str (lowest_common_ancestor (8) (8) (level) (parent))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
