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
type Heap = {
    arr: int array array
    pos_map: System.Collections.Generic.IDictionary<int, int>
    size: int
    key: int -> int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_heap (key: int -> int) =
    let mutable __ret : Heap = Unchecked.defaultof<Heap>
    let mutable key = key
    try
        __ret <- { arr = [||]; pos_map = _dictCreate []; size = 0; key = key }
        raise Return
        __ret
    with
        | Return -> __ret
let rec parent (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    try
        __ret <- if i > 0 then ((i - 1) / 2) else (-1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec left (i: int) (size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    let mutable size = size
    try
        let l: int = (2 * i) + 1
        if l < size then
            __ret <- l
            raise Return
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec right (i: int) (size: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable i = i
    let mutable size = size
    try
        let r: int = (2 * i) + 2
        if r < size then
            __ret <- r
            raise Return
        __ret <- -1
        raise Return
        __ret
    with
        | Return -> __ret
let rec swap (h: Heap) (i: int) (j: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable i = i
    let mutable j = j
    try
        let mutable arr: int array array = h.arr
        let item_i: int = _idx (_idx arr (i)) (0)
        let item_j: int = _idx (_idx arr (j)) (0)
        let mutable pm: System.Collections.Generic.IDictionary<int, int> = h.pos_map
        pm.[item_i] <- j + 1
        pm.[item_j] <- i + 1
        h <- { h with pos_map = pm }
        let tmp: int array = _idx arr (i)
        arr.[i] <- _idx arr (j)
        arr.[j] <- tmp
        h <- { h with arr = arr }
        __ret
    with
        | Return -> __ret
let rec cmp (h: Heap) (i: int) (j: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable h = h
    let mutable i = i
    let mutable j = j
    try
        let mutable arr: int array array = h.arr
        __ret <- (_idx (_idx arr (i)) (1)) < (_idx (_idx arr (j)) (1))
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_valid_parent (h: Heap) (i: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable h = h
    let mutable i = i
    try
        let mutable vp: int = i
        let l: int = left (i) (h.size)
        if (l <> (0 - 1)) && ((cmp (h) (l) (vp)) = false) then
            vp <- l
        let r: int = right (i) (h.size)
        if (r <> (0 - 1)) && ((cmp (h) (r) (vp)) = false) then
            vp <- r
        __ret <- vp
        raise Return
        __ret
    with
        | Return -> __ret
let rec heapify_up (h: Heap) (index: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable index = index
    try
        let mutable idx: int = index
        let mutable p: int = parent (idx)
        while (p <> (0 - 1)) && ((cmp (h) (idx) (p)) = false) do
            swap (h) (idx) (p)
            idx <- p
            p <- parent (p)
        __ret
    with
        | Return -> __ret
let rec heapify_down (h: Heap) (index: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable index = index
    try
        let mutable idx: int = index
        let mutable vp: int = get_valid_parent (h) (idx)
        while vp <> idx do
            swap (h) (idx) (vp)
            idx <- vp
            vp <- get_valid_parent (h) (idx)
        __ret
    with
        | Return -> __ret
let rec update_item (h: Heap) (item: int) (item_value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable item = item
    let mutable item_value = item_value
    try
        let mutable pm: System.Collections.Generic.IDictionary<int, int> = h.pos_map
        if (pm.[item]) = 0 then
            __ret <- ()
            raise Return
        let index: int = (pm.[item]) - 1
        let mutable arr: int array array = h.arr
        arr.[index] <- [|item; h.key item_value|]
        h <- { h with arr = arr }
        h <- { h with pos_map = pm }
        heapify_up (h) (index)
        heapify_down (h) (index)
        __ret
    with
        | Return -> __ret
let rec delete_item (h: Heap) (item: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable item = item
    try
        let mutable pm: System.Collections.Generic.IDictionary<int, int> = h.pos_map
        if (pm.[item]) = 0 then
            __ret <- ()
            raise Return
        let index: int = (pm.[item]) - 1
        pm.[item] <- 0
        let mutable arr: int array array = h.arr
        let last_index: int = (h.size) - 1
        if index <> last_index then
            arr.[index] <- _idx arr (last_index)
            let moved: int = _idx (_idx arr (index)) (0)
            pm.[moved] <- index + 1
        h <- { h with size = (h.size) - 1 }
        h <- { h with arr = arr }
        h <- { h with pos_map = pm }
        if (h.size) > index then
            heapify_up (h) (index)
            heapify_down (h) (index)
        __ret
    with
        | Return -> __ret
let rec insert_item (h: Heap) (item: int) (item_value: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable item = item
    let mutable item_value = item_value
    try
        let mutable arr: int array array = h.arr
        let arr_len: int = Seq.length (arr)
        if arr_len = (h.size) then
            arr <- Array.append arr [|[|item; h.key item_value|]|]
        else
            arr.[h.size] <- [|item; h.key item_value|]
        let mutable pm: System.Collections.Generic.IDictionary<int, int> = h.pos_map
        pm.[item] <- (h.size) + 1
        h <- { h with size = (h.size) + 1 }
        h <- { h with arr = arr }
        h <- { h with pos_map = pm }
        heapify_up (h) ((h.size) - 1)
        __ret
    with
        | Return -> __ret
let rec get_top (h: Heap) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable h = h
    try
        let mutable arr: int array array = h.arr
        if (h.size) > 0 then
            __ret <- _idx arr (0)
            raise Return
        __ret <- Array.empty<int>
        raise Return
        __ret
    with
        | Return -> __ret
let rec extract_top (h: Heap) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable h = h
    try
        let top: int array = get_top (h)
        if (Seq.length (top)) > 0 then
            delete_item (h) (_idx top (0))
        __ret <- top
        raise Return
        __ret
    with
        | Return -> __ret
let rec identity (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec negate (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- 0 - x
        raise Return
        __ret
    with
        | Return -> __ret
let mutable h: Heap = new_heap (unbox<int -> int> identity)
insert_item (h) (5) (34)
insert_item (h) (6) (31)
insert_item (h) (7) (37)
printfn "%s" (_str (get_top (h)))
printfn "%s" (_str (extract_top (h)))
printfn "%s" (_str (extract_top (h)))
printfn "%s" (_str (extract_top (h)))
h <- new_heap (unbox<int -> int> negate)
insert_item (h) (5) (34)
insert_item (h) (6) (31)
insert_item (h) (7) (37)
printfn "%s" (_str (get_top (h)))
printfn "%s" (_str (extract_top (h)))
printfn "%s" (_str (extract_top (h)))
printfn "%s" (_str (extract_top (h)))
insert_item (h) (8) (45)
insert_item (h) (9) (40)
insert_item (h) (10) (50)
printfn "%s" (_str (get_top (h)))
update_item (h) (10) (30)
printfn "%s" (_str (get_top (h)))
delete_item (h) (10)
printfn "%s" (_str (get_top (h)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
