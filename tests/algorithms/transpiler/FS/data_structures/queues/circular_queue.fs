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
type CircularQueue = {
    mutable _data: int array
    mutable _front: int
    mutable _rear: int
    mutable _size: int
    mutable _capacity: int
}
type DequeueResult = {
    mutable _queue: CircularQueue
    mutable _value: int
}
let rec create_queue (_capacity: int) =
    let mutable __ret : CircularQueue = Unchecked.defaultof<CircularQueue>
    let mutable _capacity = _capacity
    try
        let mutable arr: int array = [||]
        let mutable i: int = 0
        while i < _capacity do
            arr <- Array.append arr [|0|]
            i <- i + 1
        __ret <- { _data = arr; _front = 0; _rear = 0; _size = 0; _capacity = _capacity }
        raise Return
        __ret
    with
        | Return -> __ret
and length (q: CircularQueue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        __ret <- q._size
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (q: CircularQueue) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable q = q
    try
        __ret <- (q._size) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and _front (q: CircularQueue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        __ret <- if is_empty (q) then 0 else (_idx (q._data) (q._front))
        raise Return
        __ret
    with
        | Return -> __ret
and enqueue (q: CircularQueue) (_value: int) =
    let mutable __ret : CircularQueue = Unchecked.defaultof<CircularQueue>
    let mutable q = q
    let mutable _value = _value
    try
        if (q._size) >= (q._capacity) then
            failwith ("QUEUE IS FULL")
        let mutable arr: int array = q._data
        arr.[q._rear] <- _value
        q._data <- arr
        q._rear <- ((((q._rear) + 1) % (q._capacity) + (q._capacity)) % (q._capacity))
        q._size <- (q._size) + 1
        __ret <- q
        raise Return
        __ret
    with
        | Return -> __ret
and dequeue (q: CircularQueue) =
    let mutable __ret : DequeueResult = Unchecked.defaultof<DequeueResult>
    let mutable q = q
    try
        if (q._size) = 0 then
            failwith ("UNDERFLOW")
        let _value: int = _idx (q._data) (q._front)
        let mutable arr2: int array = q._data
        arr2.[q._front] <- 0
        q._data <- arr2
        q._front <- ((((q._front) + 1) % (q._capacity) + (q._capacity)) % (q._capacity))
        q._size <- (q._size) - 1
        __ret <- { _queue = q; _value = _value }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable q: CircularQueue = create_queue (5)
        printfn "%b" (is_empty (q))
        q <- enqueue (q) (10)
        printfn "%b" (is_empty (q))
        q <- enqueue (q) (20)
        q <- enqueue (q) (30)
        printfn "%A" (_front (q))
        let mutable r: DequeueResult = dequeue (q)
        q <- r._queue
        printfn "%d" (r._value)
        printfn "%A" (_front (q))
        printfn "%d" (length (q))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
