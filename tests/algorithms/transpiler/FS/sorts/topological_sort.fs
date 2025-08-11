// Generated 2025-08-11 17:23 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
open System.Collections.Generic

let edges: System.Collections.Generic.IDictionary<string, string array> = _dictCreate [("a", [|"c"; "b"|]); ("b", [|"d"; "e"|]); ("c", Array.empty<string>); ("d", Array.empty<string>); ("e", Array.empty<string>)]
let vertices: string array = unbox<string array> [|"a"; "b"; "c"; "d"; "e"|]
let rec topological_sort (start: string) (visited: System.Collections.Generic.IDictionary<string, bool>) (sort: string array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable start = start
    let mutable visited = visited
    let mutable sort = sort
    try
        visited.[start] <- true
        let neighbors: string array = _dictGet edges ((string (start)))
        let mutable i: int = 0
        while i < (Seq.length (neighbors)) do
            let neighbor: string = _idx neighbors (int i)
            if not (visited.ContainsKey(neighbor)) then
                sort <- topological_sort (neighbor) (visited) (sort)
            i <- i + 1
        sort <- Array.append sort [|start|]
        if (Seq.length (visited)) <> (Seq.length (vertices)) then
            let mutable j: int = 0
            while j < (Seq.length (vertices)) do
                let v: string = _idx vertices (int j)
                if not (visited.ContainsKey(v)) then
                    sort <- topological_sort (v) (visited) (sort)
                j <- j + 1
        __ret <- sort
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let result: string array = topological_sort ("a") (_dictCreate []) (Array.empty<string>)
        printfn "%s" (_str (result))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
