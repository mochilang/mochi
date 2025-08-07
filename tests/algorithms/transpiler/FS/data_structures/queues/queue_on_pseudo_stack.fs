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
    stack: int array
    length: int
}
type GetResult = {
    queue: Queue
    value: int
}
type FrontResult = {
    queue: Queue
    value: int
}
let rec empty_queue () =
    let mutable __ret : Queue = Unchecked.defaultof<Queue>
    try
        __ret <- { stack = [||]; length = 0 }
        raise Return
        __ret
    with
        | Return -> __ret
and put (q: Queue) (item: int) =
    let mutable __ret : Queue = Unchecked.defaultof<Queue>
    let mutable q = q
    let mutable item = item
    try
        let mutable s: int array = Array.append (q.stack) [|item|]
        __ret <- { stack = s; length = (q.length) + 1 }
        raise Return
        __ret
    with
        | Return -> __ret
and drop_first (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = [||]
        let mutable i: int = 1
        while i < (Seq.length (xs)) do
            res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and drop_last (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            res <- Array.append res [|_idx xs (i)|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and rotate (q: Queue) (rotation: int) =
    let mutable __ret : Queue = Unchecked.defaultof<Queue>
    let mutable q = q
    let mutable rotation = rotation
    try
        let mutable s: int array = q.stack
        let mutable i: int = 0
        while (i < rotation) && ((Seq.length (s)) > 0) do
            let temp: int = _idx s (0)
            s <- drop_first (s)
            s <- Array.append s [|temp|]
            i <- i + 1
        __ret <- { stack = s; length = q.length }
        raise Return
        __ret
    with
        | Return -> __ret
and get (q: Queue) =
    let mutable __ret : GetResult = Unchecked.defaultof<GetResult>
    let mutable q = q
    try
        if (q.length) = 0 then
            failwith ("queue empty")
        let mutable q1: Queue = rotate (q) (1)
        let v: int = _idx (q1.stack) ((q1.length) - 1)
        let mutable s: int array = drop_last (q1.stack)
        let mutable q2: Queue = { stack = s; length = q1.length }
        q2 <- rotate (q2) ((q2.length) - 1)
        q2 <- { stack = q2.stack; length = (q2.length) - 1 }
        __ret <- { queue = q2; value = v }
        raise Return
        __ret
    with
        | Return -> __ret
and front (q: Queue) =
    let mutable __ret : FrontResult = Unchecked.defaultof<FrontResult>
    let mutable q = q
    try
        let r: GetResult = get (q)
        let mutable q2: Queue = put (r.queue) (r.value)
        q2 <- rotate (q2) ((q2.length) - 1)
        __ret <- { queue = q2; value = r.value }
        raise Return
        __ret
    with
        | Return -> __ret
and size (q: Queue) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable q = q
    try
        __ret <- q.length
        raise Return
        __ret
    with
        | Return -> __ret
and to_string (q: Queue) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable q = q
    try
        let mutable s: string = "<"
        if (q.length) > 0 then
            s <- s + (_str (_idx (q.stack) (0)))
            let mutable i: int = 1
            while i < (q.length) do
                s <- (s + ", ") + (_str (_idx (q.stack) (i)))
                i <- i + 1
        s <- s + ">"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable q: Queue = empty_queue()
        q <- put (q) (1)
        q <- put (q) (2)
        q <- put (q) (3)
        printfn "%s" (to_string (q))
        let g: GetResult = get (q)
        q <- g.queue
        printfn "%d" (g.value)
        printfn "%s" (to_string (q))
        let f: FrontResult = front (q)
        q <- f.queue
        printfn "%d" (f.value)
        printfn "%s" (to_string (q))
        printfn "%d" (size (q))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
