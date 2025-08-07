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
    data: int
    prev_index: int
    next_index: int
}
type LinkedList = {
    nodes: Node array
    head_idx: int
    tail_idx: int
}
let rec empty_list () =
    let mutable __ret : LinkedList = Unchecked.defaultof<LinkedList>
    try
        __ret <- { nodes = [||]; head_idx = -1; tail_idx = -1 }
        raise Return
        __ret
    with
        | Return -> __ret
and get_head_data (ll: LinkedList) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ll = ll
    try
        if (ll.head_idx) = (-1) then
            __ret <- -1
            raise Return
        let node: Node = _idx (ll.nodes) (ll.head_idx)
        __ret <- node.data
        raise Return
        __ret
    with
        | Return -> __ret
and get_tail_data (ll: LinkedList) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ll = ll
    try
        if (ll.tail_idx) = (-1) then
            __ret <- -1
            raise Return
        let node: Node = _idx (ll.nodes) (ll.tail_idx)
        __ret <- node.data
        raise Return
        __ret
    with
        | Return -> __ret
and insert_before_node (ll: LinkedList) (idx: int) (new_idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ll = ll
    let mutable idx = idx
    let mutable new_idx = new_idx
    try
        let mutable nodes: Node array = ll.nodes
        let mutable new_node: Node = _idx nodes (new_idx)
        new_node <- { new_node with next_index = idx }
        let mutable node: Node = _idx nodes (idx)
        let p: int = node.prev_index
        new_node <- { new_node with prev_index = p }
        nodes.[new_idx] <- new_node
        if p = (-1) then
            ll <- { ll with head_idx = new_idx }
        else
            let mutable prev_node: Node = _idx nodes (p)
            prev_node <- { prev_node with next_index = new_idx }
            nodes.[p] <- prev_node
        node <- { node with prev_index = new_idx }
        nodes.[idx] <- node
        ll <- { ll with nodes = nodes }
        __ret
    with
        | Return -> __ret
and insert_after_node (ll: LinkedList) (idx: int) (new_idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ll = ll
    let mutable idx = idx
    let mutable new_idx = new_idx
    try
        let mutable nodes: Node array = ll.nodes
        let mutable new_node: Node = _idx nodes (new_idx)
        new_node <- { new_node with prev_index = idx }
        let mutable node: Node = _idx nodes (idx)
        let nxt: int = node.next_index
        new_node <- { new_node with next_index = nxt }
        nodes.[new_idx] <- new_node
        if nxt = (-1) then
            ll <- { ll with tail_idx = new_idx }
        else
            let mutable next_node: Node = _idx nodes (nxt)
            next_node <- { next_node with prev_index = new_idx }
            nodes.[nxt] <- next_node
        node <- { node with next_index = new_idx }
        nodes.[idx] <- node
        ll <- { ll with nodes = nodes }
        __ret
    with
        | Return -> __ret
and set_head (ll: LinkedList) (idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ll = ll
    let mutable idx = idx
    try
        if (ll.head_idx) = (-1) then
            ll <- { ll with head_idx = idx }
            ll <- { ll with tail_idx = idx }
        else
            insert_before_node (ll) (ll.head_idx) (idx)
        __ret
    with
        | Return -> __ret
and set_tail (ll: LinkedList) (idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ll = ll
    let mutable idx = idx
    try
        if (ll.tail_idx) = (-1) then
            ll <- { ll with head_idx = idx }
            ll <- { ll with tail_idx = idx }
        else
            insert_after_node (ll) (ll.tail_idx) (idx)
        __ret
    with
        | Return -> __ret
and insert (ll: LinkedList) (value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ll = ll
    let mutable value = value
    try
        let mutable nodes: Node array = ll.nodes
        nodes <- Array.append nodes [|{ data = value; prev_index = -1; next_index = -1 }|]
        let idx: int = (Seq.length (nodes)) - 1
        ll <- { ll with nodes = nodes }
        if (ll.head_idx) = (-1) then
            ll <- { ll with head_idx = idx }
            ll <- { ll with tail_idx = idx }
        else
            insert_after_node (ll) (ll.tail_idx) (idx)
        __ret
    with
        | Return -> __ret
and insert_at_position (ll: LinkedList) (position: int) (value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ll = ll
    let mutable position = position
    let mutable value = value
    try
        let mutable current: int = ll.head_idx
        let mutable current_pos: int = 1
        while current <> (-1) do
            if current_pos = position then
                let mutable nodes: Node array = ll.nodes
                nodes <- Array.append nodes [|{ data = value; prev_index = -1; next_index = -1 }|]
                let new_idx: int = (Seq.length (nodes)) - 1
                ll <- { ll with nodes = nodes }
                insert_before_node (ll) (current) (new_idx)
                __ret <- ()
                raise Return
            let node: Node = _idx (ll.nodes) (current)
            current <- node.next_index
            current_pos <- current_pos + 1
        insert (ll) (value)
        __ret
    with
        | Return -> __ret
and get_node (ll: LinkedList) (item: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ll = ll
    let mutable item = item
    try
        let mutable current: int = ll.head_idx
        while current <> (-1) do
            let node: Node = _idx (ll.nodes) (current)
            if (node.data) = item then
                __ret <- current
                raise Return
            current <- node.next_index
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
and remove_node_pointers (ll: LinkedList) (idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ll = ll
    let mutable idx = idx
    try
        let mutable nodes: Node array = ll.nodes
        let mutable node: Node = _idx nodes (idx)
        let nxt: int = node.next_index
        let p: int = node.prev_index
        if nxt <> (-1) then
            let mutable nxt_node: Node = _idx nodes (nxt)
            nxt_node <- { nxt_node with prev_index = p }
            nodes.[nxt] <- nxt_node
        if p <> (-1) then
            let mutable prev_node: Node = _idx nodes (p)
            prev_node <- { prev_node with next_index = nxt }
            nodes.[p] <- prev_node
        node <- { node with next_index = -1 }
        node <- { node with prev_index = -1 }
        nodes.[idx] <- node
        ll <- { ll with nodes = nodes }
        __ret
    with
        | Return -> __ret
and delete_value (ll: LinkedList) (value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ll = ll
    let mutable value = value
    try
        let idx: int = get_node (ll) (value)
        if idx = (-1) then
            __ret <- ()
            raise Return
        if idx = (ll.head_idx) then
            let node: Node = _idx (ll.nodes) (idx)
            ll <- { ll with head_idx = node.next_index }
        if idx = (ll.tail_idx) then
            let node: Node = _idx (ll.nodes) (idx)
            ll <- { ll with tail_idx = node.prev_index }
        remove_node_pointers (ll) (idx)
        __ret
    with
        | Return -> __ret
and contains (ll: LinkedList) (value: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ll = ll
    let mutable value = value
    try
        __ret <- (get_node (ll) (value)) <> (-1)
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (ll: LinkedList) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable ll = ll
    try
        __ret <- (ll.head_idx) = (-1)
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (ll: LinkedList) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ll = ll
    try
        let mutable res: string = ""
        let mutable first: bool = true
        let mutable current: int = ll.head_idx
        while current <> (-1) do
            let node: Node = _idx (ll.nodes) (current)
            let ``val``: string = _str (node.data)
            if first then
                res <- ``val``
                first <- false
            else
                res <- (res + " ") + ``val``
            current <- node.next_index
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and print_list (ll: LinkedList) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable ll = ll
    try
        let mutable current: int = ll.head_idx
        while current <> (-1) do
            let node: Node = _idx (ll.nodes) (current)
            printfn "%s" (_str (node.data))
            current <- node.next_index
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable ll: LinkedList = empty_list()
        printfn "%s" (_str (get_head_data (ll)))
        printfn "%s" (_str (get_tail_data (ll)))
        printfn "%s" (_str (is_empty (ll)))
        insert (ll) (10)
        printfn "%s" (_str (get_head_data (ll)))
        printfn "%s" (_str (get_tail_data (ll)))
        insert_at_position (ll) (3) (20)
        printfn "%s" (_str (get_head_data (ll)))
        printfn "%s" (_str (get_tail_data (ll)))
        let mutable nodes: Node array = ll.nodes
        nodes <- Array.append nodes [|{ data = 1000; prev_index = -1; next_index = -1 }|]
        let idx_head: int = (Seq.length (nodes)) - 1
        ll <- { ll with nodes = nodes }
        set_head (ll) (idx_head)
        nodes <- ll.nodes
        nodes <- Array.append nodes [|{ data = 2000; prev_index = -1; next_index = -1 }|]
        let idx_tail: int = (Seq.length (nodes)) - 1
        ll <- { ll with nodes = nodes }
        set_tail (ll) (idx_tail)
        print_list (ll)
        printfn "%s" (_str (is_empty (ll)))
        print_list (ll)
        printfn "%s" (_str (contains (ll) (10)))
        delete_value (ll) (10)
        printfn "%s" (_str (contains (ll) (10)))
        delete_value (ll) (2000)
        printfn "%s" (_str (get_tail_data (ll)))
        delete_value (ll) (1000)
        printfn "%s" (_str (get_tail_data (ll)))
        printfn "%s" (_str (get_head_data (ll)))
        print_list (ll)
        delete_value (ll) (20)
        print_list (ll)
        let mutable i: int = 1
        while i < 10 do
            insert (ll) (i)
            i <- i + 1
        print_list (ll)
        let mutable ll2: LinkedList = empty_list()
        insert_at_position (ll2) (1) (10)
        printfn "%s" (to_string (ll2))
        insert_at_position (ll2) (2) (20)
        printfn "%s" (to_string (ll2))
        insert_at_position (ll2) (1) (30)
        printfn "%s" (to_string (ll2))
        insert_at_position (ll2) (3) (40)
        printfn "%s" (to_string (ll2))
        insert_at_position (ll2) (5) (50)
        printfn "%s" (to_string (ll2))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
