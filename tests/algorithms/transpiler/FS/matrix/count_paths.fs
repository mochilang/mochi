// Generated 2025-08-12 09:13 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec depth_first_search (grid: int array array) (row: int) (col: int) (visit: bool array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable grid = grid
    let mutable row = row
    let mutable col = col
    let mutable visit = visit
    try
        let row_length: int = Seq.length (grid)
        let col_length: int = Seq.length (_idx grid (int 0))
        if (((row < 0) || (col < 0)) || (row = row_length)) || (col = col_length) then
            __ret <- 0
            raise Return
        if _idx (_idx visit (int row)) (int col) then
            __ret <- 0
            raise Return
        if (_idx (_idx grid (int row)) (int col)) = 1 then
            __ret <- 0
            raise Return
        if (row = (row_length - 1)) && (col = (col_length - 1)) then
            __ret <- 1
            raise Return
        visit.[row].[col] <- true
        let mutable count: int = 0
        count <- count + (depth_first_search (grid) (row + 1) (col) (visit))
        count <- count + (depth_first_search (grid) (row - 1) (col) (visit))
        count <- count + (depth_first_search (grid) (row) (col + 1) (visit))
        count <- count + (depth_first_search (grid) (row) (col - 1) (visit))
        visit.[row].[col] <- false
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and count_paths (grid: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable grid = grid
    try
        let rows: int = Seq.length (grid)
        let cols: int = Seq.length (_idx grid (int 0))
        let mutable visit: bool array array = Array.empty<bool array>
        let mutable i: int = 0
        while i < rows do
            let mutable row_visit: bool array = Array.empty<bool>
            let mutable j: int = 0
            while j < cols do
                row_visit <- Array.append row_visit [|false|]
                j <- j + 1
            visit <- Array.append visit [|row_visit|]
            i <- i + 1
        __ret <- depth_first_search (grid) (0) (0) (visit)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let grid1: int array array = [|[|0; 0; 0; 0|]; [|1; 1; 0; 0|]; [|0; 0; 0; 1|]; [|0; 1; 0; 0|]|]
        ignore (printfn "%s" (_str (count_paths (grid1))))
        let grid2: int array array = [|[|0; 0; 0; 0; 0|]; [|0; 1; 1; 1; 0|]; [|0; 1; 1; 1; 0|]; [|0; 0; 0; 0; 0|]|]
        ignore (printfn "%s" (_str (count_paths (grid2))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
