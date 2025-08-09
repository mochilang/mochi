// Generated 2025-08-09 10:14 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Node = {
    mutable _key: int
    mutable value: int
    mutable _prev: int
    mutable _next: int
}
type DoubleLinkedList = {
    mutable _nodes: Node array
    mutable _head: int
    mutable _tail: int
}
type LRUCache = {
    mutable _list: DoubleLinkedList
    mutable _capacity: int
    mutable _num_keys: int
    mutable _hits: int
    mutable _misses: int
    mutable cache: System.Collections.Generic.IDictionary<string, int>
}
type GetResult = {
    mutable cache: LRUCache
    mutable value: int
    mutable _ok: bool
}
open System.Collections.Generic

let rec new_list () =
    let mutable __ret : DoubleLinkedList = Unchecked.defaultof<DoubleLinkedList>
    try
        let mutable _nodes: Node array = Array.empty<Node>
        let _head: Node = { _key = 0; value = 0; _prev = 0 - 1; _next = 1 }
        let _tail: Node = { _key = 0; value = 0; _prev = 0; _next = 0 - 1 }
        _nodes <- Array.append _nodes [|_head|]
        _nodes <- Array.append _nodes [|_tail|]
        __ret <- { _nodes = _nodes; _head = 0; _tail = 1 }
        raise Return
        __ret
    with
        | Return -> __ret
and dll_add (lst: DoubleLinkedList) (idx: int) =
    let mutable __ret : DoubleLinkedList = Unchecked.defaultof<DoubleLinkedList>
    let mutable lst = lst
    let mutable idx = idx
    try
        let mutable _nodes: Node array = lst._nodes
        let tail_idx: int = lst._tail
        let mutable tail_node: Node = _idx _nodes (int tail_idx)
        let prev_idx: int = tail_node._prev
        let mutable node: Node = _idx _nodes (int idx)
        node._prev <- prev_idx
        node._next <- tail_idx
        _nodes.[int idx] <- node
        let mutable prev_node: Node = _idx _nodes (int prev_idx)
        prev_node._next <- idx
        _nodes.[int prev_idx] <- prev_node
        tail_node._prev <- idx
        _nodes.[int tail_idx] <- tail_node
        lst._nodes <- _nodes
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
and dll_remove (lst: DoubleLinkedList) (idx: int) =
    let mutable __ret : DoubleLinkedList = Unchecked.defaultof<DoubleLinkedList>
    let mutable lst = lst
    let mutable idx = idx
    try
        let mutable _nodes: Node array = lst._nodes
        let mutable node: Node = _idx _nodes (int idx)
        let prev_idx: int = node._prev
        let next_idx: int = node._next
        if (prev_idx = (0 - 1)) || (next_idx = (0 - 1)) then
            __ret <- lst
            raise Return
        let mutable prev_node: Node = _idx _nodes (int prev_idx)
        prev_node._next <- next_idx
        _nodes.[int prev_idx] <- prev_node
        let mutable next_node: Node = _idx _nodes (int next_idx)
        next_node._prev <- prev_idx
        _nodes.[int next_idx] <- next_node
        node._prev <- 0 - 1
        node._next <- 0 - 1
        _nodes.[int idx] <- node
        lst._nodes <- _nodes
        __ret <- lst
        raise Return
        __ret
    with
        | Return -> __ret
and new_cache (cap: int) =
    let mutable __ret : LRUCache = Unchecked.defaultof<LRUCache>
    let mutable cap = cap
    try
        let mutable empty_map: System.Collections.Generic.IDictionary<string, int> = _dictCreate []
        __ret <- { _list = new_list(); _capacity = cap; _num_keys = 0; _hits = 0; _misses = 0; cache = empty_map }
        raise Return
        __ret
    with
        | Return -> __ret
and lru_get (c: LRUCache) (_key: int) =
    let mutable __ret : GetResult = Unchecked.defaultof<GetResult>
    let mutable c = c
    let mutable _key = _key
    try
        let mutable cache: LRUCache = c
        let key_str: string = _str (_key)
        if (cache.cache).ContainsKey(key_str) then
            let idx: int = _dictGet (cache.cache) ((string (key_str)))
            if idx <> (0 - 1) then
                cache._hits <- (cache._hits) + 1
                let node: Node = _idx ((cache._list)._nodes) (int idx)
                let value: int = node.value
                cache._list <- dll_remove (cache._list) (idx)
                cache._list <- dll_add (cache._list) (idx)
                __ret <- { cache = cache; value = value; _ok = true }
                raise Return
        cache._misses <- (cache._misses) + 1
        __ret <- { cache = cache; value = 0; _ok = false }
        raise Return
        __ret
    with
        | Return -> __ret
and lru_put (c: LRUCache) (_key: int) (value: int) =
    let mutable __ret : LRUCache = Unchecked.defaultof<LRUCache>
    let mutable c = c
    let mutable _key = _key
    let mutable value = value
    try
        let mutable cache: LRUCache = c
        let key_str: string = _str (_key)
        if not ((cache.cache).ContainsKey(key_str)) then
            if (cache._num_keys) >= (cache._capacity) then
                let head_node: Node = _idx ((cache._list)._nodes) (int ((cache._list)._head))
                let first_idx: int = head_node._next
                let first_node: Node = _idx ((cache._list)._nodes) (int first_idx)
                let old_key: int = first_node._key
                cache._list <- dll_remove (cache._list) (first_idx)
                let mutable mdel: System.Collections.Generic.IDictionary<string, int> = cache.cache
                mdel.[(_str (old_key))] <- 0 - 1
                cache.cache <- mdel
                cache._num_keys <- (cache._num_keys) - 1
            let mutable _nodes: Node array = (cache._list)._nodes
            let new_node: Node = { _key = _key; value = value; _prev = 0 - 1; _next = 0 - 1 }
            _nodes <- Array.append _nodes [|new_node|]
            let idx: int = (Seq.length (_nodes)) - 1
            ((((cache :?> LRUCache)._list) :?> DoubleLinkedList)._nodes) <- _nodes
            cache._list <- dll_add (cache._list) (idx)
            let mutable m: System.Collections.Generic.IDictionary<string, int> = cache.cache
            m.[key_str] <- idx
            cache.cache <- m
            cache._num_keys <- (cache._num_keys) + 1
        else
            let mutable m: System.Collections.Generic.IDictionary<string, int> = cache.cache
            let idx: int = _dictGet m ((string (key_str)))
            let mutable _nodes: Node array = (cache._list)._nodes
            let mutable node: Node = _idx _nodes (int idx)
            node.value <- value
            _nodes.[int idx] <- node
            ((((cache :?> LRUCache)._list) :?> DoubleLinkedList)._nodes) <- _nodes
            cache._list <- dll_remove (cache._list) (idx)
            cache._list <- dll_add (cache._list) (idx)
            cache.cache <- m
        __ret <- cache
        raise Return
        __ret
    with
        | Return -> __ret
and cache_info (cache: LRUCache) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cache = cache
    try
        __ret <- ((((((("CacheInfo(hits=" + (_str (cache._hits))) + ", misses=") + (_str (cache._misses))) + ", capacity=") + (_str (cache._capacity))) + ", current size=") + (_str (cache._num_keys))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
and print_result (res: GetResult) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable res = res
    try
        if res._ok then
            printfn "%s" (_str (res.value))
        else
            printfn "%s" ("None")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable cache: LRUCache = new_cache (2)
        cache <- lru_put (cache) (1) (1)
        cache <- lru_put (cache) (2) (2)
        let mutable r1: GetResult = lru_get (cache) (1)
        cache <- r1.cache
        print_result (r1)
        cache <- lru_put (cache) (3) (3)
        let mutable r2: GetResult = lru_get (cache) (2)
        cache <- r2.cache
        print_result (r2)
        cache <- lru_put (cache) (4) (4)
        let mutable r3: GetResult = lru_get (cache) (1)
        cache <- r3.cache
        print_result (r3)
        let mutable r4: GetResult = lru_get (cache) (3)
        cache <- r4.cache
        print_result (r4)
        let mutable r5: GetResult = lru_get (cache) (4)
        cache <- r5.cache
        print_result (r5)
        printfn "%s" (cache_info (cache))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
