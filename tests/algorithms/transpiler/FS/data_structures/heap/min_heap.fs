// Generated 2025-08-07 14:57 +0700

exception Break
exception Continue

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
    name: string
    ``val``: int
}
type MinHeap = {
    heap: Node array
    idx_of_element: System.Collections.Generic.IDictionary<string, int>
    heap_dict: System.Collections.Generic.IDictionary<string, int>
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec get_parent_idx (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable idx = idx
    try
        __ret <- (idx - 1) / 2
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_left_child_idx (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable idx = idx
    try
        __ret <- (idx * 2) + 1
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_right_child_idx (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable idx = idx
    try
        __ret <- (idx * 2) + 2
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_key (m: System.Collections.Generic.IDictionary<string, int>) (k: string) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, int> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, int>>
    let mutable m = m
    let mutable k = k
    try
        let mutable out: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        for key in m.Keys do
            if key <> k then
                out.[key] <- m.[(string (key))]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec slice_without_last (xs: Node array) =
    let mutable __ret : Node array = Unchecked.defaultof<Node array>
    let mutable xs = xs
    try
        let mutable res: Node array = [||]
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec sift_down (mh: MinHeap) (idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mh = mh
    let mutable idx = idx
    try
        let mutable heap: Node array = mh.heap
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh.idx_of_element
        let mutable i: int = idx
        try
            while true do
                try
                    let left: int = get_left_child_idx (i)
                    let right: int = get_right_child_idx (i)
                    let mutable smallest: int = i
                    if (left < (Seq.length (heap))) && (((_idx heap (left)).``val``) < ((_idx heap (smallest)).``val``)) then
                        smallest <- left
                    if (right < (Seq.length (heap))) && (((_idx heap (right)).``val``) < ((_idx heap (smallest)).``val``)) then
                        smallest <- right
                    if smallest <> i then
                        let tmp: Node = _idx heap (i)
                        heap.[i] <- _idx heap (smallest)
                        heap.[smallest] <- tmp
                        idx_map.[(_idx heap (i)).name] <- i
                        idx_map.[(_idx heap (smallest)).name] <- smallest
                        i <- smallest
                    else
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        mh <- { mh with heap = heap }
        mh <- { mh with idx_of_element = idx_map }
        __ret
    with
        | Return -> __ret
let rec sift_up (mh: MinHeap) (idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mh = mh
    let mutable idx = idx
    try
        let mutable heap: Node array = mh.heap
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh.idx_of_element
        let mutable i: int = idx
        let mutable p: int = get_parent_idx (i)
        while (p >= 0) && (((_idx heap (p)).``val``) > ((_idx heap (i)).``val``)) do
            let tmp: Node = _idx heap (p)
            heap.[p] <- _idx heap (i)
            heap.[i] <- tmp
            idx_map.[(_idx heap (p)).name] <- p
            idx_map.[(_idx heap (i)).name] <- i
            i <- p
            p <- get_parent_idx (i)
        mh <- { mh with heap = heap }
        mh <- { mh with idx_of_element = idx_map }
        __ret
    with
        | Return -> __ret
let rec new_min_heap (array: Node array) =
    let mutable __ret : MinHeap = Unchecked.defaultof<MinHeap>
    let mutable array = array
    try
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable val_map: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable heap: Node array = array
        let mutable i: int = 0
        while i < (Seq.length (array)) do
            let n: Node = _idx array (i)
            idx_map.[n.name] <- i
            val_map.[n.name] <- n.``val``
            i <- i + 1
        let mutable mh: MinHeap = { heap = heap; idx_of_element = idx_map; heap_dict = val_map }
        let mutable start: int = get_parent_idx ((Seq.length (array)) - 1)
        while start >= 0 do
            sift_down (mh) (start)
            start <- start - 1
        __ret <- mh
        raise Return
        __ret
    with
        | Return -> __ret
let rec peek (mh: MinHeap) =
    let mutable __ret : Node = Unchecked.defaultof<Node>
    let mutable mh = mh
    try
        __ret <- _idx (mh.heap) (0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_min (mh: MinHeap) =
    let mutable __ret : Node = Unchecked.defaultof<Node>
    let mutable mh = mh
    try
        let mutable heap: Node array = mh.heap
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh.idx_of_element
        let mutable val_map: System.Collections.Generic.IDictionary<string, int> = mh.heap_dict
        let last_idx: int = (Seq.length (heap)) - 1
        let top: Node = _idx heap (0)
        let last: Node = _idx heap (last_idx)
        heap.[0] <- last
        idx_map.[last.name] <- 0
        heap <- slice_without_last (heap)
        idx_map <- remove_key (idx_map) (top.name)
        val_map <- remove_key (val_map) (top.name)
        mh <- { mh with heap = heap }
        mh <- { mh with idx_of_element = idx_map }
        mh <- { mh with heap_dict = val_map }
        if (Seq.length (heap)) > 0 then
            sift_down (mh) (0)
        __ret <- top
        raise Return
        __ret
    with
        | Return -> __ret
let rec insert (mh: MinHeap) (node: Node) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mh = mh
    let mutable node = node
    try
        let mutable heap: Node array = mh.heap
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh.idx_of_element
        let mutable val_map: System.Collections.Generic.IDictionary<string, int> = mh.heap_dict
        heap <- Array.append heap [|node|]
        let idx: int = (Seq.length (heap)) - 1
        idx_map.[node.name] <- idx
        val_map.[node.name] <- node.``val``
        mh <- { mh with heap = heap }
        mh <- { mh with idx_of_element = idx_map }
        mh <- { mh with heap_dict = val_map }
        sift_up (mh) (idx)
        __ret
    with
        | Return -> __ret
let rec is_empty (mh: MinHeap) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable mh = mh
    try
        __ret <- (Seq.length (mh.heap)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_value (mh: MinHeap) (key: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable mh = mh
    let mutable key = key
    try
        __ret <- mh.heap_dict.[(string (key))]
        raise Return
        __ret
    with
        | Return -> __ret
let rec decrease_key (mh: MinHeap) (node: Node) (new_value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mh = mh
    let mutable node = node
    let mutable new_value = new_value
    try
        let mutable heap: Node array = mh.heap
        let mutable val_map: System.Collections.Generic.IDictionary<string, int> = mh.heap_dict
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh.idx_of_element
        let idx: int = idx_map.[(string (node.name))]
        if not (((_idx heap (idx)).``val``) > new_value) then
            failwith ("newValue must be less than current value")
        node <- { node with ``val`` = new_value }
        ((_idx heap (idx) :?> Node).``val``) <- new_value
        val_map.[node.name] <- new_value
        mh <- { mh with heap = heap }
        mh <- { mh with heap_dict = val_map }
        sift_up (mh) (idx)
        __ret
    with
        | Return -> __ret
let rec node_to_string (n: Node) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        __ret <- ((("Node(" + (n.name)) + ", ") + (_str (n.``val``))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let mutable r: Node = { name = "R"; ``val`` = -1 }
let mutable b: Node = { name = "B"; ``val`` = 6 }
let mutable a: Node = { name = "A"; ``val`` = 3 }
let mutable x: Node = { name = "X"; ``val`` = 1 }
let mutable e: Node = { name = "E"; ``val`` = 4 }
let mutable my_min_heap: MinHeap = new_min_heap (unbox<Node array> [|r; b; a; x; e|])
printfn "%s" ("Min Heap - before decrease key")
for n in my_min_heap.heap do
    printfn "%s" (node_to_string (n))
printfn "%s" ("Min Heap - After decrease key of node [B -> -17]")
decrease_key (my_min_heap) (b) (-17)
for n in my_min_heap.heap do
    printfn "%s" (node_to_string (n))
printfn "%s" (_str (get_value (my_min_heap) ("B")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
