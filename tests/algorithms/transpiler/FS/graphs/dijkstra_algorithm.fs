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
type Edge = {
    mutable _node: int
    mutable _weight: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_int_list (n: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    let mutable value = value
    try
        let mutable lst: int array = [||]
        let mutable i: int = 0
        while i < n do
            lst <- Array.append lst [|value|]
            i <- i + 1
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_bool_list (n: int) =
    let mutable __ret : bool array = Unchecked.defaultof<bool array>
    let mutable n = n
    try
        let mutable lst: bool array = [||]
        let mutable i: int = 0
        while i < n do
            lst <- Array.append lst [|false|]
            i <- i + 1
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
let rec dijkstra (graph: Edge array array) (src: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable graph = graph
    let mutable src = src
    try
        let n: int = Seq.length (graph)
        let mutable dist: int array = make_int_list (n) (1000000000)
        let mutable visited: bool array = make_bool_list (n)
        dist <- _arrset dist (src) (0)
        let mutable count: int = 0
        try
            while count < n do
                try
                    let mutable u: int = -1
                    let mutable min_dist: int = 1000000000
                    let mutable i: int = 0
                    while i < n do
                        if (not (_idx visited (i))) && ((_idx dist (i)) < min_dist) then
                            min_dist <- _idx dist (i)
                            u <- i
                        i <- i + 1
                    if u < 0 then
                        raise Break
                    visited.[u] <- true
                    let mutable j: int = 0
                    while j < (Seq.length (_idx graph (u))) do
                        let e: Edge = _idx (_idx graph (u)) (j)
                        let v: int = e._node
                        let w: int = e._weight
                        if not (_idx visited (v)) then
                            let new_dist: int = (_idx dist (u)) + w
                            if new_dist < (_idx dist (v)) then
                                dist <- _arrset dist (v) (new_dist)
                        j <- j + 1
                    count <- count + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- dist
        raise Return
        __ret
    with
        | Return -> __ret
let graph: Edge array array = [|[|{ _node = 1; _weight = 10 }; { _node = 3; _weight = 5 }|]; [|{ _node = 2; _weight = 1 }; { _node = 3; _weight = 2 }|]; [|{ _node = 4; _weight = 4 }|]; [|{ _node = 1; _weight = 3 }; { _node = 2; _weight = 9 }; { _node = 4; _weight = 2 }|]; [|{ _node = 0; _weight = 7 }; { _node = 2; _weight = 6 }|]|]
let dist: int array = dijkstra (graph) (0)
printfn "%s" (_str (_idx dist (0)))
printfn "%s" (_str (_idx dist (1)))
printfn "%s" (_str (_idx dist (2)))
printfn "%s" (_str (_idx dist (3)))
printfn "%s" (_str (_idx dist (4)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
