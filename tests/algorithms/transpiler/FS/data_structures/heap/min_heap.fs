// Generated 2025-08-08 11:10 +0700

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
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type Node = {
    mutable _name: string
    mutable ``val``: int
}
type MinHeap = {
    mutable _heap: Node array
    mutable _idx_of_element: System.Collections.Generic.IDictionary<string, int>
    mutable _heap_dict: System.Collections.Generic.IDictionary<string, int>
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec get_parent_idx (idx: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable idx = idx
    try
        __ret <- _floordiv (idx - 1) 2
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
                out.[key] <- _dictGet m ((string (key)))
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
            res <- Array.append res [|(_idx xs (i))|]
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
        let mutable _heap: Node array = mh._heap
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh._idx_of_element
        let mutable i: int = idx
        try
            while true do
                try
                    let left: int = get_left_child_idx (i)
                    let right: int = get_right_child_idx (i)
                    let mutable smallest: int = i
                    if (left < (Seq.length (_heap))) && (((_idx _heap (left)).``val``) < ((_idx _heap (smallest)).``val``)) then
                        smallest <- left
                    if (right < (Seq.length (_heap))) && (((_idx _heap (right)).``val``) < ((_idx _heap (smallest)).``val``)) then
                        smallest <- right
                    if smallest <> i then
                        let tmp: Node = _idx _heap (i)
                        _heap.[i] <- _idx _heap (smallest)
                        _heap.[smallest] <- tmp
                        idx_map.[(_idx _heap (i))._name] <- i
                        idx_map.[(_idx _heap (smallest))._name] <- smallest
                        i <- smallest
                    else
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        mh._heap <- _heap
        mh._idx_of_element <- idx_map
        __ret
    with
        | Return -> __ret
let rec sift_up (mh: MinHeap) (idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mh = mh
    let mutable idx = idx
    try
        let mutable _heap: Node array = mh._heap
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh._idx_of_element
        let mutable i: int = idx
        let mutable p: int = get_parent_idx (i)
        while (p >= 0) && (((_idx _heap (p)).``val``) > ((_idx _heap (i)).``val``)) do
            let tmp: Node = _idx _heap (p)
            _heap.[p] <- _idx _heap (i)
            _heap.[i] <- tmp
            idx_map.[(_idx _heap (p))._name] <- p
            idx_map.[(_idx _heap (i))._name] <- i
            i <- p
            p <- get_parent_idx (i)
        mh._heap <- _heap
        mh._idx_of_element <- idx_map
        __ret
    with
        | Return -> __ret
let rec new_min_heap (array: Node array) =
    let mutable __ret : MinHeap = Unchecked.defaultof<MinHeap>
    let mutable array = array
    try
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable val_map: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        let mutable _heap: Node array = array
        let mutable i: int = 0
        while i < (Seq.length (array)) do
            let n: Node = _idx array (i)
            idx_map.[n._name] <- i
            val_map.[n._name] <- n.``val``
            i <- i + 1
        let mutable mh: MinHeap = { _heap = _heap; _idx_of_element = idx_map; _heap_dict = val_map }
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
        __ret <- _idx (mh._heap) (0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_min (mh: MinHeap) =
    let mutable __ret : Node = Unchecked.defaultof<Node>
    let mutable mh = mh
    try
        let mutable _heap: Node array = mh._heap
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh._idx_of_element
        let mutable val_map: System.Collections.Generic.IDictionary<string, int> = mh._heap_dict
        let last_idx: int = (Seq.length (_heap)) - 1
        let top: Node = _idx _heap (0)
        let last: Node = _idx _heap (last_idx)
        _heap.[0] <- last
        idx_map.[last._name] <- 0
        _heap <- slice_without_last (_heap)
        idx_map <- remove_key (idx_map) (top._name)
        val_map <- remove_key (val_map) (top._name)
        mh._heap <- _heap
        mh._idx_of_element <- idx_map
        mh._heap_dict <- val_map
        if (Seq.length (_heap)) > 0 then
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
        let mutable _heap: Node array = mh._heap
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh._idx_of_element
        let mutable val_map: System.Collections.Generic.IDictionary<string, int> = mh._heap_dict
        _heap <- Array.append _heap [|node|]
        let idx: int = (Seq.length (_heap)) - 1
        idx_map.[node._name] <- idx
        val_map.[node._name] <- node.``val``
        mh._heap <- _heap
        mh._idx_of_element <- idx_map
        mh._heap_dict <- val_map
        sift_up (mh) (idx)
        __ret
    with
        | Return -> __ret
let rec is_empty (mh: MinHeap) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable mh = mh
    try
        __ret <- (Seq.length (mh._heap)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_value (mh: MinHeap) (key: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable mh = mh
    let mutable key = key
    try
        __ret <- _dictGet (mh._heap_dict) ((string (key)))
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
        let mutable _heap: Node array = mh._heap
        let mutable val_map: System.Collections.Generic.IDictionary<string, int> = mh._heap_dict
        let mutable idx_map: System.Collections.Generic.IDictionary<string, int> = mh._idx_of_element
        let idx: int = _dictGet idx_map ((string (node._name)))
        if not (((_idx _heap (idx)).``val``) > new_value) then
            failwith ("newValue must be less than current value")
        node.``val`` <- new_value
        _heap.[idx].``val`` <- new_value
        val_map.[node._name] <- new_value
        mh._heap <- _heap
        mh._heap_dict <- val_map
        sift_up (mh) (idx)
        __ret
    with
        | Return -> __ret
let rec node_to_string (n: Node) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        __ret <- ((("Node(" + (n._name)) + ", ") + (_str (n.``val``))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let mutable r: Node = { _name = "R"; ``val`` = -1 }
let mutable b: Node = { _name = "B"; ``val`` = 6 }
let mutable a: Node = { _name = "A"; ``val`` = 3 }
let mutable x: Node = { _name = "X"; ``val`` = 1 }
let mutable e: Node = { _name = "E"; ``val`` = 4 }
let mutable my_min_heap: MinHeap = new_min_heap (unbox<Node array> [|r; b; a; x; e|])
printfn "%s" ("Min Heap - before decrease key")
for n in my_min_heap._heap do
    printfn "%s" (node_to_string (n))
printfn "%s" ("Min Heap - After decrease key of node [B -> -17]")
decrease_key (my_min_heap) (b) (-17)
for n in my_min_heap._heap do
    printfn "%s" (node_to_string (n))
printfn "%s" (_str (get_value (my_min_heap) ("B")))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
