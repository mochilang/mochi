// Generated 2025-08-08 11:10 +0700

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
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Queue = {
    mutable _entries: int array
}
type GetResult = {
    mutable _queue: Queue
    mutable _value: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_queue (items: int array) =
    let mutable __ret : Queue = Unchecked.defaultof<Queue>
    let mutable items = items
    try
        __ret <- { _entries = items }
        raise Return
        __ret
    with
        | Return -> __ret
let rec len_queue (q: Queue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        __ret <- Seq.length (q._entries)
        raise Return
        __ret
    with
        | Return -> __ret
let rec str_queue (q: Queue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable q = q
    try
        let mutable s: string = "Queue(("
        let mutable i: int = 0
        while i < (Seq.length (q._entries)) do
            s <- s + (_str (_idx (q._entries) (i)))
            if i < ((Seq.length (q._entries)) - 1) then
                s <- s + ", "
            i <- i + 1
        s <- s + "))"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec put (q: Queue) (item: int) =
    let mutable __ret : Queue = Unchecked.defaultof<Queue>
    let mutable q = q
    let mutable item = item
    try
        let mutable e: int array = q._entries
        e <- Array.append e [|item|]
        __ret <- { _entries = e }
        raise Return
        __ret
    with
        | Return -> __ret
let rec get (q: Queue) =
    let mutable __ret : GetResult = Unchecked.defaultof<GetResult>
    let mutable q = q
    try
        if (Seq.length (q._entries)) = 0 then
            failwith ("Queue is empty")
        let _value: int = _idx (q._entries) (0)
        let mutable new_entries: int array = [||]
        let mutable i: int = 1
        while i < (Seq.length (q._entries)) do
            new_entries <- Array.append new_entries [|(_idx (q._entries) (i))|]
            i <- i + 1
        __ret <- { _queue = { _entries = new_entries }; _value = _value }
        raise Return
        __ret
    with
        | Return -> __ret
let rec rotate (q: Queue) (rotation: int) =
    let mutable __ret : Queue = Unchecked.defaultof<Queue>
    let mutable q = q
    let mutable rotation = rotation
    try
        let mutable e: int array = q._entries
        let mutable r: int = 0
        while r < rotation do
            if (Seq.length (e)) > 0 then
                let first: int = _idx e (0)
                let mutable rest: int array = [||]
                let mutable i: int = 1
                while i < (Seq.length (e)) do
                    rest <- Array.append rest [|(_idx e (i))|]
                    i <- i + 1
                rest <- Array.append rest [|first|]
                e <- rest
            r <- r + 1
        __ret <- { _entries = e }
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_front (q: Queue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        __ret <- _idx (q._entries) (0)
        raise Return
        __ret
    with
        | Return -> __ret
let mutable q: Queue = new_queue (Array.empty<int>)
printfn "%d" (len_queue (q))
q <- put (q) (10)
q <- put (q) (20)
q <- put (q) (30)
q <- put (q) (40)
printfn "%s" (str_queue (q))
let res: GetResult = get (q)
q <- res._queue
printfn "%d" (res._value)
printfn "%s" (str_queue (q))
q <- rotate (q) (2)
printfn "%s" (str_queue (q))
let _front: int = get_front (q)
printfn "%d" (_front)
printfn "%s" (str_queue (q))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
