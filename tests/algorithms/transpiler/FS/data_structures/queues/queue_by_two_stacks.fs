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
type Queue = {
    stack1: int array
    stack2: int array
}
type GetResult = {
    queue: Queue
    value: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec new_queue (items: int array) =
    let mutable __ret : Queue = Unchecked.defaultof<Queue>
    let mutable items = items
    try
        __ret <- { stack1 = items; stack2 = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
let rec len_queue (q: Queue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        __ret <- (Seq.length (q.stack1)) + (Seq.length (q.stack2))
        raise Return
        __ret
    with
        | Return -> __ret
let rec str_queue (q: Queue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable q = q
    try
        let mutable items: int array = [||]
        let mutable i: int = (Seq.length (q.stack2)) - 1
        while i >= 0 do
            items <- Array.append items [|_idx (q.stack2) (i)|]
            i <- i - 1
        let mutable j: int = 0
        while j < (Seq.length (q.stack1)) do
            items <- Array.append items [|_idx (q.stack1) (j)|]
            j <- j + 1
        let mutable s: string = "Queue(("
        let mutable k: int = 0
        while k < (Seq.length (items)) do
            s <- s + (_str (_idx items (k)))
            if k < ((Seq.length (items)) - 1) then
                s <- s + ", "
            k <- k + 1
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
        let mutable s1: int array = q.stack1
        s1 <- Array.append s1 [|item|]
        __ret <- { stack1 = s1; stack2 = q.stack2 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec get (q: Queue) =
    let mutable __ret : GetResult = Unchecked.defaultof<GetResult>
    let mutable q = q
    try
        let mutable s1: int array = q.stack1
        let mutable s2: int array = q.stack2
        if (Seq.length (s2)) = 0 then
            while (Seq.length (s1)) > 0 do
                let idx: int = (Seq.length (s1)) - 1
                let v: int = _idx s1 (idx)
                let mutable new_s1: int array = [||]
                let mutable i: int = 0
                while i < idx do
                    new_s1 <- Array.append new_s1 [|_idx s1 (i)|]
                    i <- i + 1
                s1 <- new_s1
                s2 <- Array.append s2 [|v|]
        if (Seq.length (s2)) = 0 then
            failwith ("Queue is empty")
        let idx2: int = (Seq.length (s2)) - 1
        let value: int = _idx s2 (idx2)
        let mutable new_s2: int array = [||]
        let mutable j: int = 0
        while j < idx2 do
            new_s2 <- Array.append new_s2 [|_idx s2 (j)|]
            j <- j + 1
        s2 <- new_s2
        __ret <- { queue = { stack1 = s1; stack2 = s2 }; value = value }
        raise Return
        __ret
    with
        | Return -> __ret
let mutable q: Queue = new_queue (unbox<int array> [|10; 20; 30|])
let r1: GetResult = get (q)
q <- r1.queue
printfn "%d" (r1.value)
q <- put (q) (40)
let r2: GetResult = get (q)
q <- r2.queue
printfn "%d" (r2.value)
let r3: GetResult = get (q)
q <- r3.queue
printfn "%d" (r3.value)
printfn "%d" (len_queue (q))
let r4: GetResult = get (q)
q <- r4.queue
printfn "%d" (r4.value)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
