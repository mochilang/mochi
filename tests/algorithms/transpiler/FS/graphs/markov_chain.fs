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
type Transition = {
    mutable _src: string
    mutable _dst: string
    mutable _prob: float
}
open System.Collections.Generic

let mutable _seed: int = 1
let rec rand () =
    let mutable __ret : int = Unchecked.defaultof<int>
    try
        _seed <- int ((((int64 ((_seed * 1103515245) + 12345)) % 2147483648L + 2147483648L) % 2147483648L))
        __ret <- _seed
        raise Return
        __ret
    with
        | Return -> __ret
and random () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        __ret <- (1.0 * (float (rand()))) / 2147483648.0
        raise Return
        __ret
    with
        | Return -> __ret
and get_nodes (trans: Transition array) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable trans = trans
    try
        let mutable seen: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        for t in trans do
            seen.[t._src] <- true
            seen.[t._dst] <- true
        let mutable nodes: string array = [||]
        for k in seen.Keys do
            nodes <- Array.append nodes [|unbox<string> (k)|]
        __ret <- nodes
        raise Return
        __ret
    with
        | Return -> __ret
and transition (current: string) (trans: Transition array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable current = current
    let mutable trans = trans
    try
        let mutable current_probability: float = 0.0
        let random_value: float = random()
        for t in trans do
            if (t._src) = current then
                current_probability <- current_probability + (t._prob)
                if current_probability > random_value then
                    __ret <- t._dst
                    raise Return
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and get_transitions (start: string) (trans: Transition array) (steps: int) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, int> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, int>>
    let mutable start = start
    let mutable trans = trans
    let mutable steps = steps
    try
        let mutable visited: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        for node in Seq.map string (get_nodes (trans)) do
            let mutable one: int = 1
            visited.[node] <- one
        let mutable node: string = start
        let mutable i: int = 0
        while i < steps do
            node <- transition (node) (trans)
            let mutable count: int = _dictGet visited ((string (node)))
            count <- count + 1
            visited.[node] <- count
            i <- i + 1
        __ret <- visited
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let transitions: Transition array = [|{ _src = "a"; _dst = "a"; _prob = 0.9 }; { _src = "a"; _dst = "b"; _prob = 0.075 }; { _src = "a"; _dst = "c"; _prob = 0.025 }; { _src = "b"; _dst = "a"; _prob = 0.15 }; { _src = "b"; _dst = "b"; _prob = 0.8 }; { _src = "b"; _dst = "c"; _prob = 0.05 }; { _src = "c"; _dst = "a"; _prob = 0.25 }; { _src = "c"; _dst = "b"; _prob = 0.25 }; { _src = "c"; _dst = "c"; _prob = 0.5 }|]
        let result: System.Collections.Generic.IDictionary<string, int> = get_transitions ("a") (transitions) (5000)
        printfn "%s" (((((_str (_dictGet result ((string ("a"))))) + " ") + (_str (_dictGet result ((string ("b")))))) + " ") + (_str (_dictGet result ((string ("c"))))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
