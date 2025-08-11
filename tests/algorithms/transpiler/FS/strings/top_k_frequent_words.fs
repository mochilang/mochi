// Generated 2025-08-11 15:32 +0700

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
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
type WordCount = {
    mutable _word: string
    mutable _count: int
}
open System.Collections.Generic

let mutable freq_map: obj = box (_dictCreate [])
let rec heapify (arr: WordCount array) (index: int) (heap_size: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable arr = arr
    let mutable index = index
    let mutable heap_size = heap_size
    try
        let mutable largest: int = index
        let left: int = int (((int64 2) * (int64 index)) + (int64 1))
        let right: int = int (((int64 2) * (int64 index)) + (int64 2))
        if left < heap_size then
            let left_item: WordCount = _idx arr (int left)
            let largest_item: WordCount = _idx arr (int largest)
            if (left_item._count) > (largest_item._count) then
                largest <- left
        if right < heap_size then
            let right_item: WordCount = _idx arr (int right)
            let largest_item2: WordCount = _idx arr (int largest)
            if (right_item._count) > (largest_item2._count) then
                largest <- right
        if largest <> index then
            let temp: WordCount = _idx arr (int largest)
            arr.[int largest] <- _idx arr (int index)
            arr.[int index] <- temp
            heapify (arr) (largest) (heap_size)
        __ret
    with
        | Return -> __ret
and build_max_heap (arr: WordCount array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable arr = arr
    try
        let mutable i: int = (_floordiv (Seq.length (arr)) 2) - 1
        while i >= 0 do
            heapify (arr) (i) (Seq.length (arr))
            i <- i - 1
        __ret
    with
        | Return -> __ret
and top_k_frequent_words (words: string array) (k_value: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable words = words
    let mutable k_value = k_value
    try
        freq_map <- box (_dictCreate [])
        let mutable i: int = 0
        while i < (Seq.length (words)) do
            let w: string = _idx words (int i)
            if freq_map.ContainsKey(w) then
                freq_map.[w] <- ((freq_map :?> System.Collections.Generic.IDictionary<string, int>).[(w + 1)]) |> unbox<int>
            else
                freq_map.[w] <- 1
            i <- i + 1
        let mutable heap: WordCount array = Array.empty<WordCount>
        for w in freq_map do
            heap <- Array.append heap [|{ _word = unbox<string> w; _count = ((freq_map :?> System.Collections.Generic.IDictionary<string, int>).[w]) |> unbox<int> }|]
        build_max_heap (heap)
        let mutable result: string array = Array.empty<string>
        let mutable heap_size: int = Seq.length (heap)
        let mutable limit: int = k_value
        if limit > heap_size then
            limit <- heap_size
        let mutable j: int = 0
        while j < limit do
            let item: WordCount = _idx heap (int 0)
            result <- Array.append result [|(item._word)|]
            heap.[int 0] <- _idx heap (int (heap_size - 1))
            heap.[int (heap_size - 1)] <- item
            heap_size <- heap_size - 1
            heapify (heap) (0) (heap_size)
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let sample: string array = unbox<string array> [|"a"; "b"; "c"; "a"; "c"; "c"|]
        printfn "%s" (_repr (top_k_frequent_words (sample) (3)))
        printfn "%s" (_repr (top_k_frequent_words (sample) (2)))
        printfn "%s" (_repr (top_k_frequent_words (sample) (1)))
        printfn "%s" (_repr (top_k_frequent_words (sample) (0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
