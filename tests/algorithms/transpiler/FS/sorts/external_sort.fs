// Generated 2025-08-11 16:20 +0700

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
let rec subarray (xs: int array) (start: int) (``end``: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    let mutable start = start
    let mutable ``end`` = ``end``
    try
        let mutable result: int array = Array.empty<int>
        let mutable k: int = start
        while k < ``end`` do
            result <- Array.append result [|(_idx xs (int k))|]
            k <- k + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and merge (left_half: int array) (right_half: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable left_half = left_half
    let mutable right_half = right_half
    try
        let mutable result: int array = Array.empty<int>
        let mutable i: int = 0
        let mutable j: int = 0
        while (i < (Seq.length (left_half))) && (j < (Seq.length (right_half))) do
            if (_idx left_half (int i)) < (_idx right_half (int j)) then
                result <- Array.append result [|(_idx left_half (int i))|]
                i <- i + 1
            else
                result <- Array.append result [|(_idx right_half (int j))|]
                j <- j + 1
        while i < (Seq.length (left_half)) do
            result <- Array.append result [|(_idx left_half (int i))|]
            i <- i + 1
        while j < (Seq.length (right_half)) do
            result <- Array.append result [|(_idx right_half (int j))|]
            j <- j + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and merge_sort (array: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable array = array
    try
        if (Seq.length (array)) <= 1 then
            __ret <- array
            raise Return
        let middle: int = _floordiv (Seq.length (array)) 2
        let left_half: int array = subarray (array) (0) (middle)
        let right_half: int array = subarray (array) (middle) (Seq.length (array))
        let sorted_left: int array = merge_sort (left_half)
        let sorted_right: int array = merge_sort (right_half)
        __ret <- merge (sorted_left) (sorted_right)
        raise Return
        __ret
    with
        | Return -> __ret
and split_into_blocks (data: int array) (block_size: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable data = data
    let mutable block_size = block_size
    try
        let mutable blocks: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (data)) do
            let ``end``: int = if (i + block_size) < (Seq.length (data)) then (i + block_size) else (Seq.length (data))
            let block: int array = subarray (data) (i) (``end``)
            let sorted_block: int array = merge_sort (block)
            blocks <- Array.append blocks [|sorted_block|]
            i <- ``end``
        __ret <- blocks
        raise Return
        __ret
    with
        | Return -> __ret
and merge_blocks (blocks: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable blocks = blocks
    try
        let num_blocks: int = Seq.length (blocks)
        let mutable indices: int array = Array.empty<int>
        let mutable i: int = 0
        while i < num_blocks do
            indices <- Array.append indices [|0|]
            i <- i + 1
        let mutable result: int array = Array.empty<int>
        let mutable ``done``: bool = false
        while not ``done`` do
            ``done`` <- true
            let mutable min_val: int = 0
            let mutable min_block: int = 0 - 1
            let mutable j: int = 0
            while j < num_blocks do
                let idx: int = _idx indices (int j)
                if idx < (Seq.length (_idx blocks (int j))) then
                    let ``val``: int = _idx (_idx blocks (int j)) (int idx)
                    if (min_block = (0 - 1)) || (``val`` < min_val) then
                        min_val <- ``val``
                        min_block <- j
                    ``done`` <- false
                j <- j + 1
            if not ``done`` then
                result <- Array.append result [|min_val|]
                indices.[int min_block] <- (_idx indices (int min_block)) + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and external_sort (data: int array) (block_size: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable data = data
    let mutable block_size = block_size
    try
        let mutable blocks: int array array = split_into_blocks (data) (block_size)
        __ret <- merge_blocks (blocks)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let data: int array = unbox<int array> [|7; 1; 5; 3; 9; 2; 6; 4; 8; 0|]
        let sorted_data: int array = external_sort (data) (3)
        printfn "%s" (_repr (sorted_data))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
