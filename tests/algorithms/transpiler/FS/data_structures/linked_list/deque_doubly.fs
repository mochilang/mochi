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
    prev: int
    next: int
}
type LinkedDeque = {
    nodes: Node array
    header: int
    trailer: int
    size: int
}
type DeleteResult = {
    deque: LinkedDeque
    value: string
}
let rec new_deque () =
    let mutable __ret : LinkedDeque = Unchecked.defaultof<LinkedDeque>
    try
        let mutable nodes: Node array = [||]
        nodes <- Array.append nodes [|{ data = ""; prev = -1; next = 1 }|]
        nodes <- Array.append nodes [|{ data = ""; prev = 0; next = -1 }|]
        __ret <- { nodes = nodes; header = 0; trailer = 1; size = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (d: LinkedDeque) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable d = d
    try
        __ret <- (d.size) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and front (d: LinkedDeque) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable d = d
    try
        if is_empty (d) then
            failwith ("List is empty")
        let head: Node = _idx (d.nodes) (d.header)
        let idx: int = head.next
        let node: Node = _idx (d.nodes) (idx)
        __ret <- node.data
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
        let tail: Node = _idx (d.nodes) (d.trailer)
        let idx: int = tail.prev
        let node: Node = _idx (d.nodes) (idx)
        __ret <- node.data
        raise Return
        __ret
    with
        | Return -> __ret
and insert (d: LinkedDeque) (pred: int) (value: string) (succ: int) =
    let mutable __ret : LinkedDeque = Unchecked.defaultof<LinkedDeque>
    let mutable d = d
    let mutable pred = pred
    let mutable value = value
    let mutable succ = succ
    try
        let mutable nodes: Node array = d.nodes
        let new_idx: int = Seq.length (nodes)
        nodes <- Array.append nodes [|{ data = value; prev = pred; next = succ }|]
        let mutable pred_node: Node = _idx nodes (pred)
        pred_node <- { pred_node with next = new_idx }
        nodes.[pred] <- pred_node
        let mutable succ_node: Node = _idx nodes (succ)
        succ_node <- { succ_node with prev = new_idx }
        nodes.[succ] <- succ_node
        d <- { d with nodes = nodes }
        d <- { d with size = (d.size) + 1 }
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
        let mutable nodes: Node array = d.nodes
        let node: Node = _idx nodes (idx)
        let pred: int = node.prev
        let succ: int = node.next
        let mutable pred_node: Node = _idx nodes (pred)
        pred_node <- { pred_node with next = succ }
        nodes.[pred] <- pred_node
        let mutable succ_node: Node = _idx nodes (succ)
        succ_node <- { succ_node with prev = pred }
        nodes.[succ] <- succ_node
        let ``val``: string = node.data
        d <- { d with nodes = nodes }
        d <- { d with size = (d.size) - 1 }
        __ret <- { deque = d; value = ``val`` }
        raise Return
        __ret
    with
        | Return -> __ret
and add_first (d: LinkedDeque) (value: string) =
    let mutable __ret : LinkedDeque = Unchecked.defaultof<LinkedDeque>
    let mutable d = d
    let mutable value = value
    try
        let head: Node = _idx (d.nodes) (d.header)
        let succ: int = head.next
        __ret <- insert (d) (d.header) (value) (succ)
        raise Return
        __ret
    with
        | Return -> __ret
and add_last (d: LinkedDeque) (value: string) =
    let mutable __ret : LinkedDeque = Unchecked.defaultof<LinkedDeque>
    let mutable d = d
    let mutable value = value
    try
        let tail: Node = _idx (d.nodes) (d.trailer)
        let pred: int = tail.prev
        __ret <- insert (d) (pred) (value) (d.trailer)
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
        let head: Node = _idx (d.nodes) (d.header)
        let idx: int = head.next
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
        let tail: Node = _idx (d.nodes) (d.trailer)
        let idx: int = tail.prev
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
        printfn "%s" (front (d))
        d <- add_last (d) ("B")
        printfn "%s" (back (d))
        let mutable r: DeleteResult = remove_first (d)
        d <- r.deque
        printfn "%s" (r.value)
        r <- remove_last (d)
        d <- r.deque
        printfn "%s" (r.value)
        printfn "%s" (_str (is_empty (d)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
