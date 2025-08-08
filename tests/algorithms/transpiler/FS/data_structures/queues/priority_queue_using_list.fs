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
type FixedPriorityQueue = {
    mutable _queues: int array array
}
type FPQDequeueResult = {
    mutable queue: FixedPriorityQueue
    mutable value: int
}
type ElementPriorityQueue = {
    mutable queue: int array
}
type EPQDequeueResult = {
    mutable queue: ElementPriorityQueue
    mutable value: int
}
let rec panic (msg: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    try
        printfn "%s" (msg)
        __ret
    with
        | Return -> __ret
and fpq_new () =
    let mutable __ret : FixedPriorityQueue = Unchecked.defaultof<FixedPriorityQueue>
    try
        __ret <- { _queues = [|[||]; [||]; [||]|] }
        raise Return
        __ret
    with
        | Return -> __ret
and fpq_enqueue (fpq: FixedPriorityQueue) (priority: int) (data: int) =
    let mutable __ret : FixedPriorityQueue = Unchecked.defaultof<FixedPriorityQueue>
    let mutable fpq = fpq
    let mutable priority = priority
    let mutable data = data
    try
        if (priority < 0) || (priority >= (Seq.length (fpq._queues))) then
            panic ("Valid priorities are 0, 1, and 2")
            __ret <- fpq
            raise Return
        if (Seq.length (_idx (fpq._queues) (priority))) >= 100 then
            panic ("Maximum queue size is 100")
            __ret <- fpq
            raise Return
        let mutable qs: int array array = fpq._queues
        qs.[priority] <- Array.append (_idx qs (priority)) [|data|]
        fpq._queues <- qs
        __ret <- fpq
        raise Return
        __ret
    with
        | Return -> __ret
and fpq_dequeue (fpq: FixedPriorityQueue) =
    let mutable __ret : FPQDequeueResult = Unchecked.defaultof<FPQDequeueResult>
    let mutable fpq = fpq
    try
        let mutable qs: int array array = fpq._queues
        let mutable i: int = 0
        while i < (Seq.length (qs)) do
            let q: int array = _idx qs (i)
            if (Seq.length (q)) > 0 then
                let ``val``: int = _idx q (0)
                let mutable new_q: int array = [||]
                let mutable j: int = 1
                while j < (Seq.length (q)) do
                    new_q <- Array.append new_q [|(_idx q (j))|]
                    j <- j + 1
                qs.[i] <- new_q
                fpq._queues <- qs
                __ret <- { queue = fpq; value = ``val`` }
                raise Return
            i <- i + 1
        panic ("All queues are empty")
        __ret <- { queue = fpq; value = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and fpq_to_string (fpq: FixedPriorityQueue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable fpq = fpq
    try
        let mutable lines: string array = [||]
        let mutable i: int = 0
        while i < (Seq.length (fpq._queues)) do
            let mutable q_str: string = "["
            let mutable q: int array = _idx (fpq._queues) (i)
            let mutable j: int = 0
            while j < (Seq.length (q)) do
                if j > 0 then
                    q_str <- q_str + ", "
                q_str <- q_str + (_str (_idx q (j)))
                j <- j + 1
            q_str <- q_str + "]"
            lines <- Array.append lines [|((("Priority " + (_str (i))) + ": ") + q_str)|]
            i <- i + 1
        let mutable res: string = ""
        i <- 0
        while i < (Seq.length (lines)) do
            if i > 0 then
                res <- res + "\n"
            res <- res + (_idx lines (i))
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and epq_new () =
    let mutable __ret : ElementPriorityQueue = Unchecked.defaultof<ElementPriorityQueue>
    try
        __ret <- { queue = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and epq_enqueue (epq: ElementPriorityQueue) (data: int) =
    let mutable __ret : ElementPriorityQueue = Unchecked.defaultof<ElementPriorityQueue>
    let mutable epq = epq
    let mutable data = data
    try
        if (Seq.length (epq.queue)) >= 100 then
            panic ("Maximum queue size is 100")
            __ret <- epq
            raise Return
        epq.queue <- Array.append (epq.queue) [|data|]
        __ret <- epq
        raise Return
        __ret
    with
        | Return -> __ret
and epq_dequeue (epq: ElementPriorityQueue) =
    let mutable __ret : EPQDequeueResult = Unchecked.defaultof<EPQDequeueResult>
    let mutable epq = epq
    try
        if (Seq.length (epq.queue)) = 0 then
            panic ("The queue is empty")
            __ret <- { queue = epq; value = 0 }
            raise Return
        let mutable min_val: int = _idx (epq.queue) (0)
        let mutable idx: int = 0
        let mutable i: int = 1
        while i < (Seq.length (epq.queue)) do
            let v: int = _idx (epq.queue) (i)
            if v < min_val then
                min_val <- v
                idx <- i
            i <- i + 1
        let mutable new_q: int array = [||]
        i <- 0
        while i < (Seq.length (epq.queue)) do
            if i <> idx then
                new_q <- Array.append new_q [|(_idx (epq.queue) (i))|]
            i <- i + 1
        epq.queue <- new_q
        __ret <- { queue = epq; value = min_val }
        raise Return
        __ret
    with
        | Return -> __ret
and epq_to_string (epq: ElementPriorityQueue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable epq = epq
    try
        __ret <- _str (epq.queue)
        raise Return
        __ret
    with
        | Return -> __ret
and fixed_priority_queue () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable fpq: FixedPriorityQueue = fpq_new()
        fpq <- fpq_enqueue (fpq) (0) (10)
        fpq <- fpq_enqueue (fpq) (1) (70)
        fpq <- fpq_enqueue (fpq) (0) (100)
        fpq <- fpq_enqueue (fpq) (2) (1)
        fpq <- fpq_enqueue (fpq) (2) (5)
        fpq <- fpq_enqueue (fpq) (1) (7)
        fpq <- fpq_enqueue (fpq) (2) (4)
        fpq <- fpq_enqueue (fpq) (1) (64)
        fpq <- fpq_enqueue (fpq) (0) (128)
        printfn "%s" (fpq_to_string (fpq))
        let mutable res: FPQDequeueResult = fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        res <- fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        res <- fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        res <- fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        res <- fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        printfn "%s" (fpq_to_string (fpq))
        res <- fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        res <- fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        res <- fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        res <- fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        res <- fpq_dequeue (fpq)
        fpq <- res.queue
        printfn "%d" (res.value)
        __ret
    with
        | Return -> __ret
and element_priority_queue () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable epq: ElementPriorityQueue = epq_new()
        epq <- epq_enqueue (epq) (10)
        epq <- epq_enqueue (epq) (70)
        epq <- epq_enqueue (epq) (100)
        epq <- epq_enqueue (epq) (1)
        epq <- epq_enqueue (epq) (5)
        epq <- epq_enqueue (epq) (7)
        epq <- epq_enqueue (epq) (4)
        epq <- epq_enqueue (epq) (64)
        epq <- epq_enqueue (epq) (128)
        printfn "%s" (epq_to_string (epq))
        let mutable res: EPQDequeueResult = epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        res <- epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        res <- epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        res <- epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        res <- epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        printfn "%s" (epq_to_string (epq))
        res <- epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        res <- epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        res <- epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        res <- epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        res <- epq_dequeue (epq)
        epq <- res.queue
        printfn "%d" (res.value)
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        fixed_priority_queue()
        element_priority_queue()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
