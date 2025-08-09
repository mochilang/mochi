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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type LRUCache = {
    mutable _max_capacity: int
    mutable _store: string array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_cache (n: int) =
    let mutable __ret : LRUCache = Unchecked.defaultof<LRUCache>
    let mutable n = n
    try
        if n < 0 then
            failwith ("n should be an integer greater than 0.")
        let cap: int = if n = 0 then 2147483647 else n
        __ret <- { _max_capacity = cap; _store = Array.empty<string> }
        raise Return
        __ret
    with
        | Return -> __ret
let rec remove_element (xs: string array) (x: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable xs = xs
    let mutable x = x
    try
        let mutable res: string array = Array.empty<string>
        let mutable removed: bool = false
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            let v: string = _idx xs (int i)
            if (removed = false) && (v = x) then
                removed <- true
            else
                res <- unbox<string array> (Array.append res [|v|])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec refer (cache: LRUCache) (x: string) =
    let mutable __ret : LRUCache = Unchecked.defaultof<LRUCache>
    let mutable cache = cache
    let mutable x = x
    try
        let mutable _store: string array = cache._store
        let mutable exists: bool = false
        let mutable i: int = 0
        while i < (Seq.length (_store)) do
            if (_idx _store (int i)) = x then
                exists <- true
            i <- i + 1
        if exists then
            _store <- remove_element (_store) (x)
        else
            if (Seq.length (_store)) = (cache._max_capacity) then
                let mutable new_store: string array = Array.empty<string>
                let mutable j: int = 0
                while j < ((Seq.length (_store)) - 1) do
                    new_store <- unbox<string array> (Array.append new_store [|_idx _store (int j)|])
                    j <- j + 1
                _store <- new_store
        _store <- unbox<string array> (Array.append [|x|] _store)
        __ret <- { _max_capacity = cache._max_capacity; _store = _store }
        raise Return
        __ret
    with
        | Return -> __ret
let rec display (cache: LRUCache) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable cache = cache
    try
        let mutable i: int = 0
        while i < (Seq.length (cache._store)) do
            printfn "%s" (_idx (cache._store) (int i))
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec repr_item (s: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    try
        let mutable all_digits: bool = true
        let mutable i: int = 0
        while i < (String.length (s)) do
            let ch: string = string (s.[i])
            if (ch < "0") || (ch > "9") then
                all_digits <- false
            i <- i + 1
        if all_digits then
            __ret <- s
            raise Return
        __ret <- ("'" + s) + "'"
        raise Return
        __ret
    with
        | Return -> __ret
let rec cache_repr (cache: LRUCache) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cache = cache
    try
        let mutable res: string = ("LRUCache(" + (_str (cache._max_capacity))) + ") => ["
        let mutable i: int = 0
        while i < (Seq.length (cache._store)) do
            res <- res + (repr_item (_idx (cache._store) (int i)))
            if i < ((Seq.length (cache._store)) - 1) then
                res <- res + ", "
            i <- i + 1
        res <- res + "]"
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let mutable lru: LRUCache = new_cache (4)
lru <- refer (lru) ("A")
lru <- refer (lru) ("2")
lru <- refer (lru) ("3")
lru <- refer (lru) ("A")
lru <- refer (lru) ("4")
lru <- refer (lru) ("5")
let mutable r: string = cache_repr (lru)
printfn "%s" (r)
if r <> "LRUCache(4) => [5, 4, 'A', 3]" then
    failwith ("Assertion error")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
