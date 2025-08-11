// Generated 2025-08-11 16:20 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec insertion_sort (a: int array) (start: int) (end_: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable start = start
    let mutable end_ = end_
    try
        let mutable arr: int array = a
        let mutable i: int = start
        while i < end_ do
            let key: int = _idx arr (int i)
            let mutable j: int = i
            while (j > start) && ((_idx arr (int (j - 1))) > key) do
                arr.[int j] <- _idx arr (int (j - 1))
                j <- j - 1
            arr.[int j] <- key
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec heapify (a: int array) (index: int) (heap_size: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    let mutable index = index
    let mutable heap_size = heap_size
    try
        let mutable arr: int array = a
        let mutable largest: int = index
        let left: int64 = ((int64 2) * (int64 index)) + (int64 1)
        let right: int64 = ((int64 2) * (int64 index)) + (int64 2)
        if (left < (int64 heap_size)) && ((_idx arr (int left)) > (_idx arr (int largest))) then
            largest <- int left
        if (right < (int64 heap_size)) && ((_idx arr (int right)) > (_idx arr (int largest))) then
            largest <- int right
        if largest <> index then
            let temp: int = _idx arr (int index)
            arr.[int index] <- _idx arr (int largest)
            arr.[int largest] <- temp
            arr <- heapify (arr) (largest) (heap_size)
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec heap_sort (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        let mutable arr: int array = a
        let n: int = Seq.length (arr)
        if n <= 1 then
            __ret <- arr
            raise Return
        let mutable i: int = _floordiv n 2
        try
            while true do
                try
                    arr <- heapify (arr) (i) (n)
                    if i = 0 then
                        raise Break
                    i <- i - 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        i <- n - 1
        while i > 0 do
            let temp: int = _idx arr (int 0)
            arr.[int 0] <- _idx arr (int i)
            arr.[int i] <- temp
            arr <- heapify (arr) (0) (i)
            i <- i - 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec median_of_3 (arr: int array) (first: int) (middle: int) (last: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable first = first
    let mutable middle = middle
    let mutable last = last
    try
        let a: int = _idx arr (int first)
        let b: int = _idx arr (int middle)
        let c: int = _idx arr (int last)
        if ((a > b) && (a < c)) || ((a < b) && (a > c)) then
            __ret <- a
            raise Return
        else
            if ((b > a) && (b < c)) || ((b < a) && (b > c)) then
                __ret <- b
                raise Return
            else
                __ret <- c
                raise Return
        __ret
    with
        | Return -> __ret
let rec partition (arr: int array) (low: int) (high: int) (pivot: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable arr = arr
    let mutable low = low
    let mutable high = high
    let mutable pivot = pivot
    try
        let mutable i: int = low
        let mutable j: int = high
        while true do
            while (_idx arr (int i)) < pivot do
                i <- i + 1
            j <- j - 1
            while pivot < (_idx arr (int j)) do
                j <- j - 1
            if i >= j then
                __ret <- i
                raise Return
            let temp: int = _idx arr (int i)
            arr.[int i] <- _idx arr (int j)
            arr.[int j] <- temp
            i <- i + 1
        __ret
    with
        | Return -> __ret
let rec int_log2 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable v: int = n
        let mutable r: int = 0
        while v > 1 do
            v <- _floordiv v 2
            r <- r + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec intro_sort (arr: int array) (start: int) (end_: int) (size_threshold: int) (max_depth: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable arr = arr
    let mutable start = start
    let mutable end_ = end_
    let mutable size_threshold = size_threshold
    let mutable max_depth = max_depth
    try
        let mutable array: int array = arr
        let mutable s: int = start
        let mutable e: int = end_
        let mutable depth: int = max_depth
        while (e - s) > size_threshold do
            if depth = 0 then
                __ret <- heap_sort (array)
                raise Return
            depth <- depth - 1
            let pivot: int = median_of_3 (array) (s) ((s + (_floordiv (e - s) 2)) + 1) (e - 1)
            let p: int = partition (array) (s) (e) (pivot)
            array <- intro_sort (array) (p) (e) (size_threshold) (depth)
            e <- p
        let res: int array = insertion_sort (array) (s) (e)
        let _: int = Seq.length (res)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec intro_sort_main (arr: int array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable arr = arr
    try
        if (Seq.length (arr)) = 0 then
            printfn "%s" (_repr (arr))
            __ret <- ()
            raise Return
        let max_depth: int64 = (int64 2) * (int64 (int_log2 (Seq.length (arr))))
        let sorted: int array = intro_sort (arr) (0) (Seq.length (arr)) (16) (int max_depth)
        printfn "%s" (_repr (sorted))
        __ret
    with
        | Return -> __ret
let example1: int array = unbox<int array> [|4; 2; 6; 8; 1; 7; 8; 22; 14; 56; 27; 79; 23; 45; 14; 12|]
intro_sort_main (example1)
let example2: int array = unbox<int array> [|21; 15; 11; 45; -2; -11; 46|]
intro_sort_main (example2)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
