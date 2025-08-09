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
type Entry = {
    mutable _key: int
    mutable ``val``: int
    mutable _freq: int
    mutable _order: int
}
type LFUCache = {
    mutable _entries: Entry array
    mutable _capacity: int
    mutable _hits: int
    mutable _miss: int
    mutable _tick: int
}
type GetResult = {
    mutable _cache: LFUCache
    mutable _value: int
    mutable _ok: bool
}
let rec lfu_new (cap: int) =
    let mutable __ret : LFUCache = Unchecked.defaultof<LFUCache>
    let mutable cap = cap
    try
        __ret <- { _entries = Array.empty<Entry>; _capacity = cap; _hits = 0; _miss = 0; _tick = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and find_entry (_entries: Entry array) (_key: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _entries = _entries
    let mutable _key = _key
    try
        let mutable i: int = 0
        while i < (Seq.length (_entries)) do
            let e: Entry = _idx _entries (int i)
            if (e._key) = _key then
                __ret <- i
                raise Return
            i <- i + 1
        __ret <- 0 - 1
        raise Return
        __ret
    with
        | Return -> __ret
and lfu_get (_cache: LFUCache) (_key: int) =
    let mutable __ret : GetResult = Unchecked.defaultof<GetResult>
    let mutable _cache = _cache
    let mutable _key = _key
    try
        let idx: int = find_entry (_cache._entries) (_key)
        if idx = (0 - 1) then
            let new_cache: LFUCache = { _entries = _cache._entries; _capacity = _cache._capacity; _hits = _cache._hits; _miss = (_cache._miss) + 1; _tick = _cache._tick }
            __ret <- { _cache = new_cache; _value = 0; _ok = false }
            raise Return
        let mutable _entries: Entry array = _cache._entries
        let mutable e: Entry = _idx _entries (int idx)
        e._freq <- (e._freq) + 1
        let new_tick: int = (_cache._tick) + 1
        e._order <- new_tick
        _entries.[int idx] <- e
        let new_cache: LFUCache = { _entries = _entries; _capacity = _cache._capacity; _hits = (_cache._hits) + 1; _miss = _cache._miss; _tick = new_tick }
        __ret <- { _cache = new_cache; _value = e.``val``; _ok = true }
        raise Return
        __ret
    with
        | Return -> __ret
and remove_lfu (_entries: Entry array) =
    let mutable __ret : Entry array = Unchecked.defaultof<Entry array>
    let mutable _entries = _entries
    try
        if (Seq.length (_entries)) = 0 then
            __ret <- _entries
            raise Return
        let mutable min_idx: int = 0
        let mutable i: int = 1
        while i < (Seq.length (_entries)) do
            let e: Entry = _idx _entries (int i)
            let m: Entry = _idx _entries (int min_idx)
            if ((e._freq) < (m._freq)) || (((e._freq) = (m._freq)) && ((e._order) < (m._order))) then
                min_idx <- i
            i <- i + 1
        let mutable res: Entry array = Array.empty<Entry>
        let mutable j: int = 0
        while j < (Seq.length (_entries)) do
            if j <> min_idx then
                res <- Array.append res [|(_idx _entries (int j))|]
            j <- j + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and lfu_put (_cache: LFUCache) (_key: int) (_value: int) =
    let mutable __ret : LFUCache = Unchecked.defaultof<LFUCache>
    let mutable _cache = _cache
    let mutable _key = _key
    let mutable _value = _value
    try
        let mutable _entries: Entry array = _cache._entries
        let idx: int = find_entry (_entries) (_key)
        if idx <> (0 - 1) then
            let mutable e: Entry = _idx _entries (int idx)
            e.``val`` <- _value
            e._freq <- (e._freq) + 1
            let new_tick: int = (_cache._tick) + 1
            e._order <- new_tick
            _entries.[int idx] <- e
            __ret <- { _entries = _entries; _capacity = _cache._capacity; _hits = _cache._hits; _miss = _cache._miss; _tick = new_tick }
            raise Return
        if (Seq.length (_entries)) >= (_cache._capacity) then
            _entries <- remove_lfu (_entries)
        let new_tick: int = (_cache._tick) + 1
        let new_entry: Entry = { _key = _key; ``val`` = _value; _freq = 1; _order = new_tick }
        _entries <- Array.append _entries [|new_entry|]
        __ret <- { _entries = _entries; _capacity = _cache._capacity; _hits = _cache._hits; _miss = _cache._miss; _tick = new_tick }
        raise Return
        __ret
    with
        | Return -> __ret
and cache_info (_cache: LFUCache) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable _cache = _cache
    try
        __ret <- ((((((("CacheInfo(hits=" + (_str (_cache._hits))) + ", misses=") + (_str (_cache._miss))) + ", capacity=") + (_str (_cache._capacity))) + ", current_size=") + (_str (Seq.length (_cache._entries)))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable _cache: LFUCache = lfu_new (2)
        _cache <- lfu_put (_cache) (1) (1)
        _cache <- lfu_put (_cache) (2) (2)
        let mutable r: GetResult = lfu_get (_cache) (1)
        _cache <- r._cache
        if r._ok then
            printfn "%s" (_str (r._value))
        else
            printfn "%s" ("None")
        _cache <- lfu_put (_cache) (3) (3)
        r <- lfu_get (_cache) (2)
        _cache <- r._cache
        if r._ok then
            printfn "%s" (_str (r._value))
        else
            printfn "%s" ("None")
        _cache <- lfu_put (_cache) (4) (4)
        r <- lfu_get (_cache) (1)
        _cache <- r._cache
        if r._ok then
            printfn "%s" (_str (r._value))
        else
            printfn "%s" ("None")
        r <- lfu_get (_cache) (3)
        _cache <- r._cache
        if r._ok then
            printfn "%s" (_str (r._value))
        else
            printfn "%s" ("None")
        r <- lfu_get (_cache) (4)
        _cache <- r._cache
        if r._ok then
            printfn "%s" (_str (r._value))
        else
            printfn "%s" ("None")
        printfn "%s" (cache_info (_cache))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
