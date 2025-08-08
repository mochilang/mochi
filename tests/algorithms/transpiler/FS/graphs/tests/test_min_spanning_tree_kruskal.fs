// Generated 2025-08-08 16:03 +0700

exception Break
exception Continue

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
let rec sort_edges (edges: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable edges = edges
    try
        let mutable es: int array array = edges
        let mutable i: int = 0
        while i < (Seq.length (es)) do
            let mutable j: int = 0
            while j < (((Seq.length (es)) - i) - 1) do
                if (_idx (_idx es (j)) (2)) > (_idx (_idx es (j + 1)) (2)) then
                    let tmp: int array = _idx es (j)
                    es.[j] <- _idx es (j + 1)
                    es.[j + 1] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- es
        raise Return
        __ret
    with
        | Return -> __ret
and find (parent: int array) (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable parent = parent
    let mutable x = x
    try
        let mutable r: int = x
        while (_idx parent (r)) <> r do
            r <- _idx parent (r)
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and kruskal (n: int) (edges: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable n = n
    let mutable edges = edges
    try
        let mutable parent: int array = [||]
        let mutable i: int = 0
        while i < n do
            parent <- Array.append parent [|i|]
            i <- i + 1
        let mutable sorted: int array array = sort_edges (edges)
        let mutable mst: int array array = [||]
        let mutable e: int = 0
        try
            while e < (Seq.length (sorted)) do
                try
                    if (Seq.length (mst)) = (n - 1) then
                        raise Break
                    let edge: int array = _idx sorted (e)
                    e <- e + 1
                    let u: int = _idx edge (0)
                    let v: int = _idx edge (1)
                    let w: int = _idx edge (2)
                    let ru: int = find (parent) (u)
                    let rv: int = find (parent) (v)
                    if ru <> rv then
                        parent.[ru] <- rv
                        mst <- Array.append mst [|[|u; v; w|]|]
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- mst
        raise Return
        __ret
    with
        | Return -> __ret
and edges_equal (a: int array array) (b: int array array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        if (Seq.length (a)) <> (Seq.length (b)) then
            __ret <- false
            raise Return
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let e1: int array = _idx a (i)
            let e2: int array = _idx b (i)
            if (((_idx e1 (0)) <> (_idx e2 (0))) || ((_idx e1 (1)) <> (_idx e2 (1)))) || ((_idx e1 (2)) <> (_idx e2 (2))) then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let num_nodes: int = 9
        let edges: int array array = [|[|0; 1; 4|]; [|0; 7; 8|]; [|1; 2; 8|]; [|7; 8; 7|]; [|7; 6; 1|]; [|2; 8; 2|]; [|8; 6; 6|]; [|2; 3; 7|]; [|2; 5; 4|]; [|6; 5; 2|]; [|3; 5; 14|]; [|3; 4; 9|]; [|5; 4; 10|]; [|1; 7; 11|]|]
        let expected: int array array = [|[|7; 6; 1|]; [|2; 8; 2|]; [|6; 5; 2|]; [|0; 1; 4|]; [|2; 5; 4|]; [|2; 3; 7|]; [|0; 7; 8|]; [|3; 4; 9|]|]
        let result: int array array = kruskal (num_nodes) (edges)
        let sorted_result: int array array = sort_edges (result)
        let sorted_expected: int array array = sort_edges (expected)
        printfn "%s" (_str (sorted_result))
        if edges_equal (sorted_expected) (sorted_result) then
            printfn "%b" (true)
        else
            printfn "%b" (false)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
