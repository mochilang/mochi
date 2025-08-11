// Generated 2025-08-11 17:23 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec bisect_left (stacks: int array array) (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable stacks = stacks
    let mutable value = value
    try
        let mutable low: int = 0
        let mutable high: int = Seq.length (stacks)
        while low < high do
            let mid: int = _floordiv (low + high) 2
            let stack: int array = _idx stacks (int mid)
            let top_idx: int = (Seq.length (stack)) - 1
            let top: int = _idx stack (int top_idx)
            if top < value then
                low <- mid + 1
            else
                high <- mid
        __ret <- low
        raise Return
        __ret
    with
        | Return -> __ret
and reverse_list (src: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable src = src
    try
        let mutable res: int array = Array.empty<int>
        let mutable i: int = (Seq.length (src)) - 1
        while i >= 0 do
            res <- Array.append res [|(_idx src (int i))|]
            i <- i - 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and patience_sort (collection: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable collection = collection
    try
        let mutable stacks: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i < (Seq.length (collection)) do
            let element: int = _idx collection (int i)
            let idx: int = bisect_left (stacks) (element)
            if idx <> (Seq.length (stacks)) then
                let stack: int array = _idx stacks (int idx)
                stacks.[int idx] <- Array.append stack [|element|]
            else
                let mutable new_stack: int array = unbox<int array> [|element|]
                stacks <- Array.append stacks [|new_stack|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (stacks)) do
            stacks.[int i] <- reverse_list (_idx stacks (int i))
            i <- i + 1
        let mutable indices: int array = Array.empty<int>
        i <- 0
        while i < (Seq.length (stacks)) do
            indices <- Array.append indices [|0|]
            i <- i + 1
        let mutable total: int = 0
        i <- 0
        while i < (Seq.length (stacks)) do
            total <- total + (Seq.length (_idx stacks (int i)))
            i <- i + 1
        let mutable result: int array = Array.empty<int>
        let mutable count: int = 0
        while count < total do
            let mutable min_val: int = 0
            let mutable min_stack: int = -1
            let mutable j: int = 0
            while j < (Seq.length (stacks)) do
                let idx: int = _idx indices (int j)
                if idx < (Seq.length (_idx stacks (int j))) then
                    let ``val``: int = _idx (_idx stacks (int j)) (int idx)
                    if min_stack < 0 then
                        min_val <- ``val``
                        min_stack <- j
                    else
                        if ``val`` < min_val then
                            min_val <- ``val``
                            min_stack <- j
                j <- j + 1
            result <- Array.append result [|min_val|]
            indices.[int min_stack] <- (_idx indices (int min_stack)) + 1
            count <- count + 1
        i <- 0
        while i < (Seq.length (result)) do
            collection.[int i] <- _idx result (int i)
            i <- i + 1
        __ret <- collection
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (patience_sort (unbox<int array> [|1; 9; 5; 21; 17; 6|])))
printfn "%s" (_str (patience_sort (Array.empty<int>)))
printfn "%s" (_str (patience_sort (unbox<int array> [|-3; -17; -48|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
