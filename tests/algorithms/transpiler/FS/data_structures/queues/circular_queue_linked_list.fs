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
type CircularQueue = {
    data: string array
    next: int array
    prev: int array
    front: int
    rear: int
}
type DequeueResult = {
    queue: CircularQueue
    value: string
}
let rec create_queue (capacity: int) =
    let mutable __ret : CircularQueue = Unchecked.defaultof<CircularQueue>
    let mutable capacity = capacity
    try
        let mutable data: string array = [||]
        let mutable next: int array = [||]
        let mutable prev: int array = [||]
        let mutable i: int = 0
        while i < capacity do
            data <- Array.append data [|""|]
            next <- Array.append next [|(((i + 1) % capacity + capacity) % capacity)|]
            prev <- Array.append prev [|((((i - 1) + capacity) % capacity + capacity) % capacity)|]
            i <- i + 1
        __ret <- { data = data; next = next; prev = prev; front = 0; rear = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (q: CircularQueue) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable q = q
    try
        __ret <- ((q.front) = (q.rear)) && ((_idx (q.data) (q.front)) = "")
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
        if (_idx (q.next) (q.rear)) = (q.front) then
            failwith ("Full Queue")
        __ret
    with
        | Return -> __ret
and peek (q: CircularQueue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable q = q
    try
        check_can_perform (q)
        __ret <- _idx (q.data) (q.front)
        raise Return
        __ret
    with
        | Return -> __ret
and enqueue (q: CircularQueue) (value: string) =
    let mutable __ret : CircularQueue = Unchecked.defaultof<CircularQueue>
    let mutable q = q
    let mutable value = value
    try
        check_is_full (q)
        if not (is_empty (q)) then
            q <- { q with rear = _idx (q.next) (q.rear) }
        let mutable data: string array = q.data
        data.[q.rear] <- value
        q <- { q with data = data }
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
        let mutable data: string array = q.data
        let ``val``: string = _idx data (q.front)
        data.[q.front] <- ""
        q <- { q with data = data }
        if (q.front) <> (q.rear) then
            q <- { q with front = _idx (q.next) (q.front) }
        __ret <- { queue = q; value = ``val`` }
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
        q <- res.queue
        printfn "%s" (res.value)
        res <- dequeue (q)
        q <- res.queue
        printfn "%s" (res.value)
        printfn "%s" (_str (is_empty (q)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
