// Generated 2025-08-08 18:58 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec is_safe (grid: int array array) (visited: bool array array) (row: int) (col: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable grid = grid
    let mutable visited = visited
    let mutable row = row
    let mutable col = col
    try
        let rows: int = Seq.length (grid)
        let cols: int = Seq.length (_idx grid (int 0))
        let visited_cell: bool = _idx (_idx visited (int row)) (int col)
        let within_bounds: bool = (((row >= 0) && (row < rows)) && (col >= 0)) && (col < cols)
        let not_visited: bool = visited_cell = false
        __ret <- (within_bounds && not_visited) && ((_idx (_idx grid (int row)) (int col)) = 1)
        raise Return
        __ret
    with
        | Return -> __ret
let rec dfs (grid: int array array) (visited: bool array array) (row: int) (col: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable grid = grid
    let mutable visited = visited
    let mutable row = row
    let mutable col = col
    try
        let row_nbr: int array = unbox<int array> [|-1; -1; -1; 0; 0; 1; 1; 1|]
        let col_nbr: int array = unbox<int array> [|-1; 0; 1; -1; 1; -1; 0; 1|]
        visited.[int row].[int col] <- true
        let mutable k: int = 0
        while k < 8 do
            let new_row: int = row + (_idx row_nbr (int k))
            let new_col: int = col + (_idx col_nbr (int k))
            if is_safe (grid) (visited) (new_row) (new_col) then
                dfs (grid) (visited) (new_row) (new_col)
            k <- k + 1
        __ret
    with
        | Return -> __ret
let rec count_islands (grid: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable grid = grid
    try
        let rows: int = Seq.length (grid)
        let cols: int = Seq.length (_idx grid (int 0))
        let mutable visited: bool array array = Array.empty<bool array>
        let mutable i: int = 0
        while i < rows do
            let mutable row_list: bool array = Array.empty<bool>
            let mutable j: int = 0
            while j < cols do
                row_list <- Array.append row_list [|false|]
                j <- j + 1
            visited <- Array.append visited [|row_list|]
            i <- i + 1
        let mutable count: int = 0
        i <- 0
        while i < rows do
            let mutable j: int = 0
            while j < cols do
                if (not (_idx (_idx visited (int i)) (int j))) && ((_idx (_idx grid (int i)) (int j)) = 1) then
                    dfs (grid) (visited) (i) (j)
                    count <- count + 1
                j <- j + 1
            i <- i + 1
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
let grid: int array array = [|[|1; 1; 0; 0; 0|]; [|0; 1; 0; 0; 1|]; [|1; 0; 0; 1; 1|]; [|0; 0; 0; 0; 0|]; [|1; 0; 1; 0; 1|]|]
printfn "%d" (count_islands (grid))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
