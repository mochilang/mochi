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
open System.Collections.Generic

let mutable tree: System.Collections.Generic.IDictionary<int, int array> = _dictCreate []
let rec dfs (start: int) (visited: System.Collections.Generic.IDictionary<int, bool>) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable start = start
    let mutable visited = visited
    try
        let mutable size: int = 1
        let mutable cuts: int = 0
        visited.[start] <- true
        for v in _dictGet tree (start) do
            if not (visited.ContainsKey(v)) then
                let res: int array = dfs (v) (visited)
                size <- size + (_idx res (0))
                cuts <- cuts + (_idx res (1))
        if (((size % 2 + 2) % 2)) = 0 then
            cuts <- cuts + 1
        __ret <- unbox<int array> [|size; cuts|]
        raise Return
        __ret
    with
        | Return -> __ret
and even_tree () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        let visited: System.Collections.Generic.IDictionary<int, bool> = _dictCreate []
        let res: int array = dfs (1) (visited)
        __ret <- (_idx res (1)) - 1
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let edges: int array array = [|[|2; 1|]; [|3; 1|]; [|4; 3|]; [|5; 2|]; [|6; 1|]; [|7; 2|]; [|8; 6|]; [|9; 8|]; [|10; 8|]|]
        let mutable i: int = 0
        while i < (Seq.length (edges)) do
            let u: int = _idx (_idx edges (i)) (0)
            let v: int = _idx (_idx edges (i)) (1)
            if not (tree.ContainsKey(u)) then
                tree.[u] <- [||]
            if not (tree.ContainsKey(v)) then
                tree.[v] <- [||]
            tree.[u] <- Array.append (_dictGet tree (u)) [|v|]
            tree.[v] <- Array.append (_dictGet tree (v)) [|u|]
            i <- i + 1
        printfn "%s" (_str (even_tree()))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
