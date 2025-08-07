// Generated 2025-08-07 15:46 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec create_bool_matrix (rows: int) (cols: int) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable rows = rows
    let mutable cols = cols
    try
        let mutable matrix: bool array array = [||]
        let mutable i: int = 0
        while i <= rows do
            let mutable row: bool array = [||]
            let mutable j: int = 0
            while j <= cols do
                row <- Array.append row [|false|]
                j <- j + 1
            matrix <- Array.append matrix [|row|]
            i <- i + 1
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_sum_subset (arr: int array) (required_sum: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable arr = arr
    let mutable required_sum = required_sum
    try
        let arr_len: int = Seq.length (arr)
        let mutable subset: bool array array = create_bool_matrix (arr_len) (required_sum)
        let mutable i: int = 0
        while i <= arr_len do
            subset.[i].[0] <- true
            i <- i + 1
        let mutable j: int = 1
        while j <= required_sum do
            subset.[0].[j] <- false
            j <- j + 1
        i <- 1
        while i <= arr_len do
            j <- 1
            while j <= required_sum do
                if (_idx arr (i - 1)) > j then
                    subset.[i].[j] <- _idx (_idx subset (i - 1)) (j)
                if (_idx arr (i - 1)) <= j then
                    subset.[i].[j] <- (_idx (_idx subset (i - 1)) (j)) || (_idx (_idx subset (i - 1)) (j - (_idx arr (i - 1))))
                j <- j + 1
            i <- i + 1
        __ret <- _idx (_idx subset (arr_len)) (required_sum)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%b" (is_sum_subset (unbox<int array> [|2; 4; 6; 8|]) (5))
printfn "%b" (is_sum_subset (unbox<int array> [|2; 4; 6; 8|]) (14))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
