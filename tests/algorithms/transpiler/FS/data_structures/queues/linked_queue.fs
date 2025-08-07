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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Node = {
    data: string
    next: int
}
type LinkedQueue = {
    nodes: Node array
    front: int
    rear: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_queue () =
    let mutable __ret : LinkedQueue = Unchecked.defaultof<LinkedQueue>
    try
        __ret <- { nodes = [||]; front = 0 - 1; rear = 0 - 1 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_empty (q: LinkedQueue) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable q = q
    try
        __ret <- (q.front) = (0 - 1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec put (q: LinkedQueue) (item: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable q = q
    let mutable item = item
    try
        let node: Node = { data = item; next = 0 - 1 }
        q <- { q with nodes = Array.append (q.nodes) [|node|] }
        let mutable idx: int = (Seq.length (q.nodes)) - 1
        if (q.front) = (0 - 1) then
            q <- { q with front = idx }
            q <- { q with rear = idx }
        else
            let mutable nodes: Node array = q.nodes
            ((_idx nodes (q.rear) :?> Node).next) <- idx
            q <- { q with nodes = nodes }
            q <- { q with rear = idx }
        __ret
    with
        | Return -> __ret
let rec get (q: LinkedQueue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable q = q
    try
        if is_empty (q) then
            failwith ("dequeue from empty queue")
        let mutable idx: int = q.front
        let node: Node = _idx (q.nodes) (idx)
        q <- { q with front = node.next }
        if (q.front) = (0 - 1) then
            q <- { q with rear = 0 - 1 }
        __ret <- node.data
        raise Return
        __ret
    with
        | Return -> __ret
let rec length (q: LinkedQueue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        let mutable count: int = 0
        let mutable idx: int = q.front
        while idx <> (0 - 1) do
            count <- count + 1
            idx <- (_idx (q.nodes) (idx)).next
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
        let mutable idx: int = q.front
        let mutable first: bool = true
        while idx <> (0 - 1) do
            let node: Node = _idx (q.nodes) (idx)
            if first then
                res <- node.data
                first <- false
            else
                res <- (res + " <- ") + (node.data)
            idx <- node.next
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec clear (q: LinkedQueue) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable q = q
    try
        q <- { q with nodes = [||] }
        q <- { q with front = 0 - 1 }
        q <- { q with rear = 0 - 1 }
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
