// Generated 2025-08-07 14:57 +0700

exception Break
exception Continue

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
type BinomialHeap = {
    data: int array
}
type DeleteResult = {
    heap: BinomialHeap
    value: int
}
let rec new_heap () =
    let mutable __ret : BinomialHeap = Unchecked.defaultof<BinomialHeap>
    try
        __ret <- { data = [||] }
        raise Return
        __ret
    with
        | Return -> __ret
and swap (data: int array) (i: int) (j: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable data = data
    let mutable i = i
    let mutable j = j
    try
        let tmp: int = _idx data (i)
        data.[i] <- _idx data (j)
        data.[j] <- tmp
        __ret
    with
        | Return -> __ret
and sift_up (data: int array) (idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable data = data
    let mutable idx = idx
    try
        let mutable i: int = idx
        try
            while i > 0 do
                try
                    let parent: int = (i - 1) / 2
                    if (_idx data (parent)) <= (_idx data (i)) then
                        raise Break
                    swap (data) (parent) (i)
                    i <- parent
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and sift_down (data: int array) (idx: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable data = data
    let mutable idx = idx
    try
        let mutable i: int = idx
        let n: int = Seq.length (data)
        try
            while true do
                try
                    let left: int = (2 * i) + 1
                    let right: int = left + 1
                    let mutable smallest: int = i
                    if (left < n) && ((_idx data (left)) < (_idx data (smallest))) then
                        smallest <- left
                    if (right < n) && ((_idx data (right)) < (_idx data (smallest))) then
                        smallest <- right
                    if smallest = i then
                        raise Break
                    swap (data) (i) (smallest)
                    i <- smallest
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and insert (heap: BinomialHeap) (v: int) =
    let mutable __ret : BinomialHeap = Unchecked.defaultof<BinomialHeap>
    let mutable heap = heap
    let mutable v = v
    try
        let mutable d: int array = heap.data
        d <- Array.append d [|v|]
        sift_up (d) ((Seq.length (d)) - 1)
        __ret <- { data = d }
        raise Return
        __ret
    with
        | Return -> __ret
and peek (heap: BinomialHeap) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable heap = heap
    try
        __ret <- _idx (heap.data) (0)
        raise Return
        __ret
    with
        | Return -> __ret
and is_empty (heap: BinomialHeap) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable heap = heap
    try
        __ret <- (Seq.length (heap.data)) = 0
        raise Return
        __ret
    with
        | Return -> __ret
and delete_min (heap: BinomialHeap) =
    let mutable __ret : DeleteResult = Unchecked.defaultof<DeleteResult>
    let mutable heap = heap
    try
        let mutable d: int array = heap.data
        let min: int = _idx d (0)
        d.[0] <- _idx d ((Seq.length (d)) - 1)
        d <- Array.sub d 0 (((Seq.length (d)) - 1) - 0)
        if (Seq.length (d)) > 0 then
            sift_down (d) (0)
        __ret <- { heap = { data = d }; value = min }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable h: BinomialHeap = new_heap()
        h <- insert (h) (10)
        h <- insert (h) (3)
        h <- insert (h) (7)
        printfn "%s" (_str (peek (h)))
        let d1: DeleteResult = delete_min (h)
        h <- d1.heap
        printfn "%s" (_str (d1.value))
        let d2: DeleteResult = delete_min (h)
        h <- d2.heap
        printfn "%s" (_str (d2.value))
        let d3: DeleteResult = delete_min (h)
        h <- d3.heap
        printfn "%s" (_str (d3.value))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
