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
type CircularQueue = {
    mutable _data: string array
    mutable _next: int array
    mutable _prev: int array
    mutable _front: int
    mutable _rear: int
}
type DequeueResult = {
    mutable _queue: CircularQueue
    mutable _value: string
}
let rec create_queue (capacity: int) =
    let mutable __ret : CircularQueue = Unchecked.defaultof<CircularQueue>
    let mutable capacity = capacity
    try
        let mutable _data: string array = [||]
        let mutable _next: int array = [||]
        let mutable _prev: int array = [||]
        let mutable i: int = 0
        while i < capacity do
            _data <- Array.append _data [|""|]
            _next <- Array.append _next [|((((i + 1) % capacity + capacity) % capacity))|]
            _prev <- Array.append _prev [|(((((i - 1) + capacity) % capacity + capacity) % capacity))|]
            i <- i + 1
        __ret <- { _data = _data; _next = _next; _prev = _prev; _front = 0; _rear = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (q: CircularQueue) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable q = q
    try
        __ret <- ((q._front) = (q._rear)) && ((_idx (q._data) (q._front)) = "")
        raise Return
        __ret
    with
        | Return -> __ret
and check_can_perform (q: CircularQueue) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable q = q
    try
        if is_empty (q) then
            failwith ("Empty Queue")
        __ret
    with
        | Return -> __ret
and check_is_full (q: CircularQueue) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable q = q
    try
        if (_idx (q._next) (q._rear)) = (q._front) then
            failwith ("Full Queue")
        __ret
    with
        | Return -> __ret
and peek (q: CircularQueue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable q = q
    try
        check_can_perform (q)
        __ret <- _idx (q._data) (q._front)
        raise Return
        __ret
    with
        | Return -> __ret
and enqueue (q: CircularQueue) (_value: string) =
    let mutable __ret : CircularQueue = Unchecked.defaultof<CircularQueue>
    let mutable q = q
    let mutable _value = _value
    try
        check_is_full (q)
        if not (is_empty (q)) then
            q._rear <- _idx (q._next) (q._rear)
        let mutable _data: string array = q._data
        _data.[q._rear] <- _value
        q._data <- _data
        __ret <- q
        raise Return
        __ret
    with
        | Return -> __ret
and dequeue (q: CircularQueue) =
    let mutable __ret : DequeueResult = Unchecked.defaultof<DequeueResult>
    let mutable q = q
    try
        check_can_perform (q)
        let mutable _data: string array = q._data
        let ``val``: string = _idx _data (q._front)
        _data.[q._front] <- ""
        q._data <- _data
        if (q._front) <> (q._rear) then
            q._front <- _idx (q._next) (q._front)
        __ret <- { _queue = q; _value = ``val`` }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable q: CircularQueue = create_queue (3)
        printfn "%s" (_str (is_empty (q)))
        q <- enqueue (q) ("a")
        q <- enqueue (q) ("b")
        printfn "%s" (peek (q))
        let mutable res: DequeueResult = dequeue (q)
        q <- res._queue
        printfn "%s" (res._value)
        res <- dequeue (q)
        q <- res._queue
        printfn "%s" (res._value)
        printfn "%s" (_str (is_empty (q)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
