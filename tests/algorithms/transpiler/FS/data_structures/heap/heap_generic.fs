// Generated 2025-08-24 23:17 +0700

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
    match box v with
    | :? float as f -> sprintf "%.10g" f
    | :? int64 as n -> sprintf "%d" n
    | _ ->
        let s = sprintf "%A" v
        s.Replace("[|", "[")
         .Replace("|]", "]")
         .Replace("; ", " ")
         .Replace(";", "")
         .Replace("L", "")
         .Replace("\"", "")
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
type Heap = {
    mutable _arr: int64 array array
    mutable _pos_map: System.Collections.Generic.IDictionary<int64, int64>
    mutable _size: int64
    mutable _key: int64 -> int64
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec new_heap (_key: int64 -> int64) =
    let mutable __ret : Heap = Unchecked.defaultof<Heap>
    let mutable _key = _key
    try
        __ret <- { _arr = Array.empty<int64 array>; _pos_map = _dictCreate<int64, int64> []; _size = int64 0; _key = _key }
        raise Return
        __ret
    with
        | Return -> __ret
and parent (i: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable i = i
    try
        __ret <- if i > (int64 0) then (_floordiv64 (int64 (i - (int64 1))) (int64 (int64 2))) else (int64 (-1))
        raise Return
        __ret
    with
        | Return -> __ret
and left (i: int64) (_size: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable i = i
    let mutable _size = _size
    try
        let l: int64 = ((int64 2) * i) + (int64 1)
        if l < _size then
            __ret <- l
            raise Return
        __ret <- int64 (-1)
        raise Return
        __ret
    with
        | Return -> __ret
and right (i: int64) (_size: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable i = i
    let mutable _size = _size
    try
        let r: int64 = ((int64 2) * i) + (int64 2)
        if r < _size then
            __ret <- r
            raise Return
        __ret <- int64 (-1)
        raise Return
        __ret
    with
        | Return -> __ret
and swap (h: Heap) (i: int64) (j: int64) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable i = i
    let mutable j = j
    try
        let mutable _arr: int64 array array = h._arr
        let item_i: int64 = _idx (_idx _arr (int i)) (int 0)
        let item_j: int64 = _idx (_idx _arr (int j)) (int 0)
        let mutable pm: System.Collections.Generic.IDictionary<int64, int64> = h._pos_map
        pm <- _dictAdd (pm) (item_i) (j + (int64 1))
        pm <- _dictAdd (pm) (item_j) (i + (int64 1))
        h._pos_map <- pm
        let tmp: int64 array = _idx _arr (int i)
        _arr.[int i] <- _idx _arr (int j)
        _arr.[int j] <- tmp
        h._arr <- _arr
        __ret
    with
        | Return -> __ret
and cmp (h: Heap) (i: int64) (j: int64) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable h = h
    let mutable i = i
    let mutable j = j
    try
        let mutable _arr: int64 array array = h._arr
        __ret <- (_idx (_idx _arr (int i)) (int 1)) < (_idx (_idx _arr (int j)) (int 1))
        raise Return
        __ret
    with
        | Return -> __ret
and get_valid_parent (h: Heap) (i: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable h = h
    let mutable i = i
    try
        let mutable vp: int64 = i
        let l: int64 = left (i) (h._size)
        if (l <> (int64 (0 - 1))) && ((cmp (h) (l) (vp)) = false) then
            vp <- l
        let r: int64 = right (i) (h._size)
        if (r <> (int64 (0 - 1))) && ((cmp (h) (r) (vp)) = false) then
            vp <- r
        __ret <- vp
        raise Return
        __ret
    with
        | Return -> __ret
and heapify_up (h: Heap) (index: int64) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable index = index
    try
        let mutable idx: int64 = index
        let mutable p: int64 = parent (idx)
        while (p <> (int64 (0 - 1))) && ((cmp (h) (idx) (p)) = false) do
            swap (h) (idx) (p)
            idx <- p
            p <- parent (p)
        __ret
    with
        | Return -> __ret
and heapify_down (h: Heap) (index: int64) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable index = index
    try
        let mutable idx: int64 = index
        let mutable vp: int64 = get_valid_parent (h) (idx)
        while vp <> idx do
            swap (h) (idx) (vp)
            idx <- vp
            vp <- get_valid_parent (h) (idx)
        __ret
    with
        | Return -> __ret
and update_item (h: Heap) (item: int64) (item_value: int64) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable item = item
    let mutable item_value = item_value
    try
        let mutable pm: System.Collections.Generic.IDictionary<int64, int64> = h._pos_map
        if (_dictGet pm (item)) = (int64 0) then
            __ret <- ()
            raise Return
        let index: int64 = (_dictGet pm (item)) - (int64 1)
        let mutable _arr: int64 array array = h._arr
        _arr.[int index] <- unbox<int64 array> [|item; h._key item_value|]
        h._arr <- _arr
        h._pos_map <- pm
        heapify_up (h) (index)
        heapify_down (h) (index)
        __ret
    with
        | Return -> __ret
and delete_item (h: Heap) (item: int64) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable item = item
    try
        let mutable pm: System.Collections.Generic.IDictionary<int64, int64> = h._pos_map
        if (_dictGet pm (item)) = (int64 0) then
            __ret <- ()
            raise Return
        let index: int64 = (_dictGet pm (item)) - (int64 1)
        pm <- _dictAdd (pm) (item) (int64 0)
        let mutable _arr: int64 array array = h._arr
        let last_index: int64 = (h._size) - (int64 1)
        if index <> last_index then
            _arr.[int index] <- _idx _arr (int last_index)
            let moved: int64 = _idx (_idx _arr (int index)) (int 0)
            pm <- _dictAdd (pm) (moved) (index + (int64 1))
        h._size <- (h._size) - (int64 1)
        h._arr <- _arr
        h._pos_map <- pm
        if (h._size) > index then
            heapify_up (h) (index)
            heapify_down (h) (index)
        __ret
    with
        | Return -> __ret
and insert_item (h: Heap) (item: int64) (item_value: int64) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable item = item
    let mutable item_value = item_value
    try
        let mutable _arr: int64 array array = h._arr
        let arr_len: int64 = int64 (Seq.length (_arr))
        if arr_len = (h._size) then
            _arr <- Array.append _arr [|[|item; h._key item_value|]|]
        else
            _arr.[int (h._size)] <- unbox<int64 array> [|item; h._key item_value|]
        let mutable pm: System.Collections.Generic.IDictionary<int64, int64> = h._pos_map
        pm <- _dictAdd (pm) (item) ((h._size) + (int64 1))
        h._size <- (h._size) + (int64 1)
        h._arr <- _arr
        h._pos_map <- pm
        heapify_up (h) ((h._size) - (int64 1))
        __ret
    with
        | Return -> __ret
and get_top (h: Heap) =
    let mutable __ret : int64 array = Unchecked.defaultof<int64 array>
    let mutable h = h
    try
        let mutable _arr: int64 array array = h._arr
        if (h._size) > (int64 0) then
            __ret <- _idx _arr (int 0)
            raise Return
        __ret <- Array.empty<int64>
        raise Return
        __ret
    with
        | Return -> __ret
and extract_top (h: Heap) =
    let mutable __ret : int64 array = Unchecked.defaultof<int64 array>
    let mutable h = h
    try
        let top: int64 array = get_top (h)
        if (Seq.length (top)) > 0 then
            delete_item (h) (_idx top (int 0))
        __ret <- top
        raise Return
        __ret
    with
        | Return -> __ret
and identity (x: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable x = x
    try
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and negate (x: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable x = x
    try
        __ret <- (int64 0) - x
        raise Return
        __ret
    with
        | Return -> __ret
let mutable h: Heap = new_heap (unbox<int64 -> int64> identity)
insert_item (h) (int64 5) (int64 34)
insert_item (h) (int64 6) (int64 31)
insert_item (h) (int64 7) (int64 37)
ignore (printfn "%s" (_str (get_top (h))))
ignore (printfn "%s" (_str (extract_top (h))))
ignore (printfn "%s" (_str (extract_top (h))))
ignore (printfn "%s" (_str (extract_top (h))))
h <- new_heap (unbox<int64 -> int64> negate)
insert_item (h) (int64 5) (int64 34)
insert_item (h) (int64 6) (int64 31)
insert_item (h) (int64 7) (int64 37)
ignore (printfn "%s" (_str (get_top (h))))
ignore (printfn "%s" (_str (extract_top (h))))
ignore (printfn "%s" (_str (extract_top (h))))
ignore (printfn "%s" (_str (extract_top (h))))
insert_item (h) (int64 8) (int64 45)
insert_item (h) (int64 9) (int64 40)
insert_item (h) (int64 10) (int64 50)
ignore (printfn "%s" (_str (get_top (h))))
update_item (h) (int64 10) (int64 30)
ignore (printfn "%s" (_str (get_top (h))))
delete_item (h) (int64 10)
ignore (printfn "%s" (_str (get_top (h))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
