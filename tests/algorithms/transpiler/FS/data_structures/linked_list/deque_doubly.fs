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
    mutable _data: string
    mutable _prev: int
    mutable _next: int
}
type LinkedDeque = {
    mutable _nodes: Node array
    mutable _header: int
    mutable _trailer: int
    mutable _size: int
}
type DeleteResult = {
    mutable _deque: LinkedDeque
    mutable _value: string
}
let rec new_deque () =
    let mutable __ret : LinkedDeque = Unchecked.defaultof<LinkedDeque>
    try
        let mutable _nodes: Node array = [||]
        _nodes <- Array.append _nodes [|{ _data = ""; _prev = -1; _next = 1 }|]
        _nodes <- Array.append _nodes [|{ _data = ""; _prev = 0; _next = -1 }|]
        __ret <- { _nodes = _nodes; _header = 0; _trailer = 1; _size = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (d: LinkedDeque) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable d = d
    try
        __ret <- (d._size) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and _front (d: LinkedDeque) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable d = d
    try
        if is_empty (d) then
            failwith ("List is empty")
        let head: Node = _idx (d._nodes) (d._header)
        let idx: int = head._next
        let node: Node = _idx (d._nodes) (idx)
        __ret <- node._data
        raise Return
        __ret
    with
        | Return -> __ret
and back (d: LinkedDeque) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable d = d
    try
        if is_empty (d) then
            failwith ("List is empty")
        let tail: Node = _idx (d._nodes) (d._trailer)
        let idx: int = tail._prev
        let node: Node = _idx (d._nodes) (idx)
        __ret <- node._data
        raise Return
        __ret
    with
        | Return -> __ret
and insert (d: LinkedDeque) (pred: int) (_value: string) (succ: int) =
    let mutable __ret : LinkedDeque = Unchecked.defaultof<LinkedDeque>
    let mutable d = d
    let mutable pred = pred
    let mutable _value = _value
    let mutable succ = succ
    try
        let mutable _nodes: Node array = d._nodes
        let new_idx: int = Seq.length (_nodes)
        _nodes <- Array.append _nodes [|{ _data = _value; _prev = pred; _next = succ }|]
        let mutable pred_node: Node = _idx _nodes (pred)
        pred_node._next <- new_idx
        _nodes.[pred] <- pred_node
        let mutable succ_node: Node = _idx _nodes (succ)
        succ_node._prev <- new_idx
        _nodes.[succ] <- succ_node
        d._nodes <- _nodes
        d._size <- (d._size) + 1
        __ret <- d
        raise Return
        __ret
    with
        | Return -> __ret
and delete (d: LinkedDeque) (idx: int) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable d = d
    let mutable idx = idx
    try
        let mutable _nodes: Node array = d._nodes
        let node: Node = _idx _nodes (idx)
        let pred: int = node._prev
        let succ: int = node._next
        let mutable pred_node: Node = _idx _nodes (pred)
        pred_node._next <- succ
        _nodes.[pred] <- pred_node
        let mutable succ_node: Node = _idx _nodes (succ)
        succ_node._prev <- pred
        _nodes.[succ] <- succ_node
        let ``val``: string = node._data
        d._nodes <- _nodes
        d._size <- (d._size) - 1
        __ret <- { _deque = d; _value = ``val`` }
        raise Return
        __ret
    with
        | Return -> __ret
and add_first (d: LinkedDeque) (_value: string) =
    let mutable __ret : LinkedDeque = Unchecked.defaultof<LinkedDeque>
    let mutable d = d
    let mutable _value = _value
    try
        let head: Node = _idx (d._nodes) (d._header)
        let succ: int = head._next
        __ret <- insert (d) (d._header) (_value) (succ)
        raise Return
        __ret
    with
        | Return -> __ret
and add_last (d: LinkedDeque) (_value: string) =
    let mutable __ret : LinkedDeque = Unchecked.defaultof<LinkedDeque>
    let mutable d = d
    let mutable _value = _value
    try
        let tail: Node = _idx (d._nodes) (d._trailer)
        let pred: int = tail._prev
        __ret <- insert (d) (pred) (_value) (d._trailer)
        raise Return
        __ret
    with
        | Return -> __ret
and remove_first (d: LinkedDeque) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable d = d
    try
        if is_empty (d) then
            failwith ("remove_first from empty list")
        let head: Node = _idx (d._nodes) (d._header)
        let idx: int = head._next
        __ret <- delete (d) (idx)
        raise Return
        __ret
    with
        | Return -> __ret
and remove_last (d: LinkedDeque) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable d = d
    try
        if is_empty (d) then
            failwith ("remove_first from empty list")
        let tail: Node = _idx (d._nodes) (d._trailer)
        let idx: int = tail._prev
        __ret <- delete (d) (idx)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable d: LinkedDeque = new_deque()
        d <- add_first (d) ("A")
        printfn "%A" (_front (d))
        d <- add_last (d) ("B")
        printfn "%s" (back (d))
        let mutable r: DeleteResult = remove_first (d)
        d <- r._deque
        printfn "%s" (r._value)
        r <- remove_last (d)
        d <- r._deque
        printfn "%s" (r._value)
        printfn "%s" (_str (is_empty (d)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
