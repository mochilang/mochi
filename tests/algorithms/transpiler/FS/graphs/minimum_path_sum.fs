// Generated 2025-08-08 16:03 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec fill_row (current_row: int array) (row_above: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable current_row = current_row
    let mutable row_above = row_above
    try
        current_row.[0] <- (_idx current_row (0)) + (_idx row_above (0))
        let mutable cell_n: int = 1
        while cell_n < (Seq.length (current_row)) do
            let left: int = _idx current_row (cell_n - 1)
            let up: int = _idx row_above (cell_n)
            if left < up then
                current_row.[cell_n] <- (_idx current_row (cell_n)) + left
            else
                current_row.[cell_n] <- (_idx current_row (cell_n)) + up
            cell_n <- cell_n + 1
        __ret <- current_row
        raise Return
        __ret
    with
        | Return -> __ret
let rec min_path_sum (grid: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable grid = grid
    try
        if ((Seq.length (grid)) = 0) || ((Seq.length (_idx grid (0))) = 0) then
            failwith ("The grid does not contain the appropriate information")
        let mutable cell_n: int = 1
        while cell_n < (Seq.length (_idx grid (0))) do
            grid.[0].[cell_n] <- (_idx (_idx grid (0)) (cell_n)) + (_idx (_idx grid (0)) (cell_n - 1))
            cell_n <- cell_n + 1
        let mutable row_above: int array = _idx grid (0)
        let mutable row_n: int = 1
        while row_n < (Seq.length (grid)) do
            let current_row: int array = _idx grid (row_n)
            grid.[row_n] <- fill_row (current_row) (row_above)
            row_above <- _idx grid (row_n)
            row_n <- row_n + 1
        __ret <- _idx (_idx grid ((Seq.length (grid)) - 1)) ((Seq.length (_idx grid (0))) - 1)
        raise Return
        __ret
    with
        | Return -> __ret
let grid1: int array array = [|[|1; 3; 1|]; [|1; 5; 1|]; [|4; 2; 1|]|]
printfn "%s" (_str (min_path_sum (grid1)))
let grid2: int array array = [|[|1; 0; 5; 6; 7|]; [|8; 9; 0; 4; 2|]; [|4; 4; 4; 5; 1|]; [|9; 6; 3; 1; 0|]; [|8; 4; 3; 2; 7|]|]
printfn "%s" (_str (min_path_sum (grid2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
