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
type Node = {
    mutable _name: string
    mutable _inbound: string array
    mutable _outbound: string array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec node_to_string (n: Node) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        __ret <- ((((("<node=" + (n._name)) + " inbound=") + (n._inbound)) + " outbound=") + (n._outbound)) + ">"
        raise Return
        __ret
    with
        | Return -> __ret
let rec page_rank (nodes: Node array) (limit: int) (d: float) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    let mutable nodes = nodes
    let mutable limit = limit
    let mutable d = d
    try
        let mutable ranks: System.Collections.Generic.IDictionary<string, float> = _dictCreate []
        for n in nodes do
            ranks.[n._name] <- 1.0
        let mutable outbounds: System.Collections.Generic.IDictionary<string, float> = _dictCreate []
        for n in nodes do
            outbounds.[n._name] <- 1.0 * (float (Seq.length (n._outbound)))
        let mutable i: int = 0
        while i < limit do
            printfn "%s" (("======= Iteration " + (_str (i + 1))) + " =======")
            for n in nodes do
                let mutable sum_val: float = 0.0
                for ib in Seq.map string (n._inbound) do
                    sum_val <- sum_val + ((_dictGet ranks ((string (ib)))) / (_dictGet outbounds ((string (ib)))))
                ranks.[n._name] <- (1.0 - d) + (d * sum_val)
            printfn "%A" (ranks)
            i <- i + 1
        __ret <- ranks
        raise Return
        __ret
    with
        | Return -> __ret
let names: string array = [|"A"; "B"; "C"|]
let graph: int array array = [|[|0; 1; 1|]; [|0; 0; 1|]; [|1; 0; 0|]|]
let mutable nodes: Node array = [||]
for _name in Seq.map string (names) do
    nodes <- Array.append nodes [|{ _name = _name; _inbound = [||]; _outbound = [||] }|]
let mutable ri: int = 0
while ri < (Seq.length (graph)) do
    let mutable row: int array = _idx graph (ri)
    let mutable ci: int = 0
    while ci < (Seq.length (row)) do
        if (_idx row (ci)) = 1 then
            let mutable n_in: Node = _idx nodes (ci)
            n_in._inbound <- Array.append (n_in._inbound) [|(_idx names (ri))|]
            nodes <- _arrset nodes (ci) (n_in)
            let mutable n_out: Node = _idx nodes (ri)
            n_out._outbound <- Array.append (n_out._outbound) [|(_idx names (ci))|]
            nodes <- _arrset nodes (ri) (n_out)
        ci <- ci + 1
    ri <- ri + 1
printfn "%s" ("======= Nodes =======")
for n in nodes do
    printfn "%A" (n)
page_rank (nodes) (3) (0.85)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
