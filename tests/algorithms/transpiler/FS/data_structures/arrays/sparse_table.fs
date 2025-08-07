// Generated 2025-08-07 10:31 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pow2 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < n do
            result <- result * 2
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec int_log2 (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable v: int = n
        let mutable res: int = 0
        while v > 1 do
            v <- v / 2
            res <- res + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec build_sparse_table (number_list: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable number_list = number_list
    try
        if (Seq.length (number_list)) = 0 then
            failwith ("empty number list not allowed")
        let length: int = Seq.length (number_list)
        let row: int = (int_log2 (length)) + 1
        let mutable sparse_table: int array array = [||]
        let mutable j: int = 0
        while j < row do
            let mutable inner: int array = [||]
            let mutable i: int = 0
            while i < length do
                inner <- Array.append inner [|0|]
                i <- i + 1
            sparse_table <- Array.append sparse_table [|inner|]
            j <- j + 1
        let mutable i: int = 0
        while i < length do
            sparse_table.[0].[i] <- _idx number_list (i)
            i <- i + 1
        j <- 1
        while (pow2 (j)) <= length do
            i <- 0
            while ((i + (pow2 (j))) - 1) < length do
                let left: int = _idx (_idx sparse_table (j - 1)) (i + (pow2 (j - 1)))
                let right: int = _idx (_idx sparse_table (j - 1)) (i)
                if left < right then
                    sparse_table.[j].[i] <- left
                else
                    sparse_table.[j].[i] <- right
                i <- i + 1
            j <- j + 1
        __ret <- sparse_table
        raise Return
        __ret
    with
        | Return -> __ret
let rec query (sparse_table: int array array) (left_bound: int) (right_bound: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable sparse_table = sparse_table
    let mutable left_bound = left_bound
    let mutable right_bound = right_bound
    try
        if (left_bound < 0) || (right_bound >= (Seq.length (_idx sparse_table (0)))) then
            failwith ("list index out of range")
        let interval: int = (right_bound - left_bound) + 1
        let mutable j: int = int_log2 (interval)
        let val1: int = _idx (_idx sparse_table (j)) ((right_bound - (pow2 (j))) + 1)
        let val2: int = _idx (_idx sparse_table (j)) (left_bound)
        if val1 < val2 then
            __ret <- val1
            raise Return
        __ret <- val2
        raise Return
        __ret
    with
        | Return -> __ret
let st1: int array array = build_sparse_table (unbox<int array> [|8; 1; 0; 3; 4; 9; 3|])
printfn "%s" (_str (st1))
let st2: int array array = build_sparse_table (unbox<int array> [|3; 1; 9|])
printfn "%s" (_str (st2))
printfn "%s" (_str (query (st1) (0) (4)))
printfn "%s" (_str (query (st1) (4) (6)))
printfn "%s" (_str (query (st2) (2) (2)))
printfn "%s" (_str (query (st2) (0) (1)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
