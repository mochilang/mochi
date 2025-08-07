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
type Entry = {
    key: string
    value: string
}
type HashMap = {
    entries: Entry array
}
type GetResult = {
    found: bool
    value: string
}
type DelResult = {
    map: HashMap
    ok: bool
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_hash_map () =
    let mutable __ret : HashMap = Unchecked.defaultof<HashMap>
    try
        __ret <- { entries = Array.empty<Entry> }
        raise Return
        __ret
    with
        | Return -> __ret
let rec hm_len (m: HashMap) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable m = m
    try
        __ret <- Seq.length (m.entries)
        raise Return
        __ret
    with
        | Return -> __ret
let rec hm_set (m: HashMap) (key: string) (value: string) =
    let mutable __ret : HashMap = Unchecked.defaultof<HashMap>
    let mutable m = m
    let mutable key = key
    let mutable value = value
    try
        let mutable entries: Entry array = m.entries
        let mutable updated: bool = false
        let mutable new_entries: Entry array = Array.empty<Entry>
        let mutable i: int = 0
        while i < (Seq.length (entries)) do
            let e: Entry = _idx entries (i)
            if (e.key) = key then
                new_entries <- Array.append new_entries [|{ key = key; value = value }|]
                updated <- true
            else
                new_entries <- Array.append new_entries [|e|]
            i <- i + 1
        if not updated then
            new_entries <- Array.append new_entries [|{ key = key; value = value }|]
        __ret <- { entries = new_entries }
        raise Return
        __ret
    with
        | Return -> __ret
let rec hm_get (m: HashMap) (key: string) =
    let mutable __ret : GetResult = Unchecked.defaultof<GetResult>
    let mutable m = m
    let mutable key = key
    try
        let mutable i: int = 0
        while i < (Seq.length (m.entries)) do
            let e: Entry = _idx (m.entries) (i)
            if (e.key) = key then
                __ret <- { found = true; value = e.value }
                raise Return
            i <- i + 1
        __ret <- { found = false; value = "" }
        raise Return
        __ret
    with
        | Return -> __ret
let rec hm_del (m: HashMap) (key: string) =
    let mutable __ret : DelResult = Unchecked.defaultof<DelResult>
    let mutable m = m
    let mutable key = key
    try
        let mutable entries: Entry array = m.entries
        let mutable new_entries: Entry array = Array.empty<Entry>
        let mutable removed: bool = false
        let mutable i: int = 0
        while i < (Seq.length (entries)) do
            let e: Entry = _idx entries (i)
            if (e.key) = key then
                removed <- true
            else
                new_entries <- Array.append new_entries [|e|]
            i <- i + 1
        if removed then
            __ret <- { map = { entries = new_entries }; ok = true }
            raise Return
        __ret <- { map = m; ok = false }
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_add_items () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let mutable h: HashMap = make_hash_map()
        h <- hm_set (h) ("key_a") ("val_a")
        h <- hm_set (h) ("key_b") ("val_b")
        let a: GetResult = hm_get (h) ("key_a")
        let b: GetResult = hm_get (h) ("key_b")
        __ret <- (((((hm_len (h)) = 2) && (a.found)) && (b.found)) && ((a.value) = "val_a")) && ((b.value) = "val_b")
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_overwrite_items () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let mutable h: HashMap = make_hash_map()
        h <- hm_set (h) ("key_a") ("val_a")
        h <- hm_set (h) ("key_a") ("val_b")
        let a: GetResult = hm_get (h) ("key_a")
        __ret <- (((hm_len (h)) = 1) && (a.found)) && ((a.value) = "val_b")
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_delete_items () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let mutable h: HashMap = make_hash_map()
        h <- hm_set (h) ("key_a") ("val_a")
        h <- hm_set (h) ("key_b") ("val_b")
        let d1: DelResult = hm_del (h) ("key_a")
        h <- d1.map
        let d2: DelResult = hm_del (h) ("key_b")
        h <- d2.map
        h <- hm_set (h) ("key_a") ("val_a")
        let d3: DelResult = hm_del (h) ("key_a")
        h <- d3.map
        __ret <- (hm_len (h)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_access_absent_items () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let mutable h: HashMap = make_hash_map()
        let g1: GetResult = hm_get (h) ("key_a")
        let d1: DelResult = hm_del (h) ("key_a")
        h <- d1.map
        h <- hm_set (h) ("key_a") ("val_a")
        let d2: DelResult = hm_del (h) ("key_a")
        h <- d2.map
        let d3: DelResult = hm_del (h) ("key_a")
        h <- d3.map
        let g2: GetResult = hm_get (h) ("key_a")
        __ret <- ((((((g1.found) = false) && ((d1.ok) = false)) && (d2.ok)) && ((d3.ok) = false)) && ((g2.found) = false)) && ((hm_len (h)) = 0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_add_with_resize_up () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let mutable h: HashMap = make_hash_map()
        let mutable i: int = 0
        while i < 5 do
            let s: string = _str (i)
            h <- hm_set (h) (s) (s)
            i <- i + 1
        __ret <- (hm_len (h)) = 5
        raise Return
        __ret
    with
        | Return -> __ret
let rec test_add_with_resize_down () =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    try
        let mutable h: HashMap = make_hash_map()
        let mutable i: int = 0
        while i < 5 do
            let s: string = _str (i)
            h <- hm_set (h) (s) (s)
            i <- i + 1
        let mutable j: int = 0
        while j < 5 do
            let s: string = _str (j)
            let d: DelResult = hm_del (h) (s)
            h <- d.map
            j <- j + 1
        h <- hm_set (h) ("key_a") ("val_b")
        let a: GetResult = hm_get (h) ("key_a")
        __ret <- (((hm_len (h)) = 1) && (a.found)) && ((a.value) = "val_b")
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%b" (test_add_items())
printfn "%b" (test_overwrite_items())
printfn "%b" (test_delete_items())
printfn "%b" (test_access_absent_items())
printfn "%b" (test_add_with_resize_up())
printfn "%b" (test_add_with_resize_down())
printfn "%b" (true)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
