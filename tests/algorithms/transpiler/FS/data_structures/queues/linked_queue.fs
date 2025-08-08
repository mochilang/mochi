// Generated 2025-08-08 11:10 +0700

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
type Node = {
    mutable _data: string
    mutable _next: int
}
type LinkedQueue = {
    mutable _nodes: Node array
    mutable _front: int
    mutable _rear: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_queue () =
    let mutable __ret : LinkedQueue = Unchecked.defaultof<LinkedQueue>
    try
        __ret <- { _nodes = [||]; _front = 0 - 1; _rear = 0 - 1 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_empty (q: LinkedQueue) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable q = q
    try
        __ret <- (q._front) = (0 - 1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec put (q: LinkedQueue) (item: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable q = q
    let mutable item = item
    try
        let node: Node = { _data = item; _next = 0 - 1 }
        q._nodes <- Array.append (q._nodes) [|node|]
        let mutable idx: int = (Seq.length (q._nodes)) - 1
        if (q._front) = (0 - 1) then
            q._front <- idx
            q._rear <- idx
        else
            let mutable _nodes: Node array = q._nodes
            _nodes.[q._rear]._next <- idx
            q._nodes <- _nodes
            q._rear <- idx
        __ret
    with
        | Return -> __ret
let rec get (q: LinkedQueue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable q = q
    try
        if is_empty (q) then
            failwith ("dequeue from empty queue")
        let mutable idx: int = q._front
        let node: Node = _idx (q._nodes) (idx)
        q._front <- node._next
        if (q._front) = (0 - 1) then
            q._rear <- 0 - 1
        __ret <- node._data
        raise Return
        __ret
    with
        | Return -> __ret
let rec length (q: LinkedQueue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        let mutable count: int = 0
        let mutable idx: int = q._front
        while idx <> (0 - 1) do
            count <- count + 1
            idx <- (_idx (q._nodes) (idx))._next
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_string (q: LinkedQueue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable q = q
    try
        let mutable res: string = ""
        let mutable idx: int = q._front
        let mutable first: bool = true
        while idx <> (0 - 1) do
            let node: Node = _idx (q._nodes) (idx)
            if first then
                res <- node._data
                first <- false
            else
                res <- (res + " <- ") + (node._data)
            idx <- node._next
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec clear (q: LinkedQueue) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable q = q
    try
        q._nodes <- [||]
        q._front <- 0 - 1
        q._rear <- 0 - 1
        __ret
    with
        | Return -> __ret
let queue: LinkedQueue = new_queue()
printfn "%s" (_str (is_empty (queue)))
put (queue) ("5")
put (queue) ("9")
put (queue) ("python")
printfn "%s" (_str (is_empty (queue)))
printfn "%s" (get (queue))
put (queue) ("algorithms")
printfn "%s" (get (queue))
printfn "%s" (get (queue))
printfn "%s" (get (queue))
printfn "%s" (_str (is_empty (queue)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
