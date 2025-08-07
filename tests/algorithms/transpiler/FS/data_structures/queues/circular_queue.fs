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
type CircularQueue = {
    data: int array
    front: int
    rear: int
    size: int
    capacity: int
}
type DequeueResult = {
    queue: CircularQueue
    value: int
}
let rec create_queue (capacity: int) =
    let mutable __ret : CircularQueue = Unchecked.defaultof<CircularQueue>
    let mutable capacity = capacity
    try
        let mutable arr: int array = [||]
        let mutable i: int = 0
        while i < capacity do
            arr <- Array.append arr [|0|]
            i <- i + 1
        __ret <- { data = arr; front = 0; rear = 0; size = 0; capacity = capacity }
        raise Return
        __ret
    with
        | Return -> __ret
and length (q: CircularQueue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        __ret <- q.size
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (q: CircularQueue) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable q = q
    try
        __ret <- (q.size) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and front (q: CircularQueue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        __ret <- if is_empty (q) then 0 else (_idx (q.data) (q.front))
        raise Return
        __ret
    with
        | Return -> __ret
and enqueue (q: CircularQueue) (value: int) =
    let mutable __ret : CircularQueue = Unchecked.defaultof<CircularQueue>
    let mutable q = q
    let mutable value = value
    try
        if (q.size) >= (q.capacity) then
            failwith ("QUEUE IS FULL")
        let mutable arr: int array = q.data
        arr.[q.rear] <- value
        q <- { q with data = arr }
        q <- { q with rear = ((((q.rear) + 1) % (q.capacity) + (q.capacity)) % (q.capacity)) }
        q <- { q with size = (q.size) + 1 }
        __ret <- q
        raise Return
        __ret
    with
        | Return -> __ret
and dequeue (q: CircularQueue) =
    let mutable __ret : DequeueResult = Unchecked.defaultof<DequeueResult>
    let mutable q = q
    try
        if (q.size) = 0 then
            failwith ("UNDERFLOW")
        let value: int = _idx (q.data) (q.front)
        let mutable arr2: int array = q.data
        arr2.[q.front] <- 0
        q <- { q with data = arr2 }
        q <- { q with front = ((((q.front) + 1) % (q.capacity) + (q.capacity)) % (q.capacity)) }
        q <- { q with size = (q.size) - 1 }
        __ret <- { queue = q; value = value }
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
        printfn "%d" (front (q))
        let mutable r: DequeueResult = dequeue (q)
        q <- r.queue
        printfn "%d" (r.value)
        printfn "%d" (front (q))
        printfn "%d" (length (q))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
