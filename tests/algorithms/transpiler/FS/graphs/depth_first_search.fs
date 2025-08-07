// Generated 2025-08-07 16:27 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec contains (lst: string array) (v: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable lst = lst
    let mutable v = v
    try
        let mutable i: int = 0
        while i < (Seq.length (lst)) do
            if (_idx lst (i)) = v then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
let rec depth_first_search (graph: System.Collections.Generic.IDictionary<string, string array>) (start: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable graph = graph
    let mutable start = start
    try
        let mutable explored: string array = [||]
        let mutable stack: string array = [||]
        stack <- Array.append stack [|start|]
        explored <- Array.append explored [|start|]
        while (Seq.length (stack)) > 0 do
            let idx: int = (Seq.length (stack)) - 1
            let v: string = _idx stack (idx)
            stack <- Array.sub stack 0 (idx - 0)
            let neighbors: string array = _dictGet graph ((string (v)))
            let mutable i: int = (Seq.length (neighbors)) - 1
            while i >= 0 do
                let adj: string = _idx neighbors (i)
                if not (contains (explored) (adj)) then
                    explored <- Array.append explored [|adj|]
                    stack <- Array.append stack [|adj|]
                i <- i - 1
        __ret <- explored
        raise Return
        __ret
    with
        | Return -> __ret
let G: System.Collections.Generic.IDictionary<string, string array> = _dictCreate [("A", [|"B"; "C"; "D"|]); ("B", [|"A"; "D"; "E"|]); ("C", [|"A"; "F"|]); ("D", [|"B"; "D"|]); ("E", [|"B"; "F"|]); ("F", [|"C"; "E"; "G"|]); ("G", [|"F"|])]
let result: string array = depth_first_search (G) ("A")
printfn "%s" (_repr (result))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
