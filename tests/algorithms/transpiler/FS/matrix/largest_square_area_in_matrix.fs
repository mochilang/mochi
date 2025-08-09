// Generated 2025-08-09 10:14 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec update_area_of_max_square (row: int) (col: int) (rows: int) (cols: int) (mat: int array array) (largest_square_area: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable row = row
    let mutable col = col
    let mutable rows = rows
    let mutable cols = cols
    let mutable mat = mat
    let mutable largest_square_area = largest_square_area
    try
        if (row >= rows) || (col >= cols) then
            __ret <- 0
            raise Return
        let right: int = update_area_of_max_square (row) (col + 1) (rows) (cols) (mat) (largest_square_area)
        let diagonal: int = update_area_of_max_square (row + 1) (col + 1) (rows) (cols) (mat) (largest_square_area)
        let down: int = update_area_of_max_square (row + 1) (col) (rows) (cols) (mat) (largest_square_area)
        if (_idx (_idx mat (int row)) (int col)) = 1 then
            let sub = 1 + (int (Array.min ([|right; diagonal; down|])))
            if (int sub) > (_idx largest_square_area (int 0)) then
                largest_square_area.[int 0] <- sub
            __ret <- int sub
            raise Return
        else
            __ret <- 0
            raise Return
        __ret
    with
        | Return -> __ret
let rec largest_square_area_in_matrix_top_down (rows: int) (cols: int) (mat: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable rows = rows
    let mutable cols = cols
    let mutable mat = mat
    try
        let mutable largest: int array = unbox<int array> [|0|]
        update_area_of_max_square (0) (0) (rows) (cols) (mat) (largest)
        __ret <- _idx largest (int 0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec update_area_of_max_square_with_dp (row: int) (col: int) (rows: int) (cols: int) (mat: int array array) (dp_array: int array array) (largest_square_area: int array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable row = row
    let mutable col = col
    let mutable rows = rows
    let mutable cols = cols
    let mutable mat = mat
    let mutable dp_array = dp_array
    let mutable largest_square_area = largest_square_area
    try
        if (row >= rows) || (col >= cols) then
            __ret <- 0
            raise Return
        if (_idx (_idx dp_array (int row)) (int col)) <> (-1) then
            __ret <- _idx (_idx dp_array (int row)) (int col)
            raise Return
        let right: int = update_area_of_max_square_with_dp (row) (col + 1) (rows) (cols) (mat) (dp_array) (largest_square_area)
        let diagonal: int = update_area_of_max_square_with_dp (row + 1) (col + 1) (rows) (cols) (mat) (dp_array) (largest_square_area)
        let down: int = update_area_of_max_square_with_dp (row + 1) (col) (rows) (cols) (mat) (dp_array) (largest_square_area)
        if (_idx (_idx mat (int row)) (int col)) = 1 then
            let sub = 1 + (int (Array.min ([|right; diagonal; down|])))
            if (int sub) > (_idx largest_square_area (int 0)) then
                largest_square_area.[int 0] <- sub
            dp_array.[int row].[int col] <- sub
            __ret <- int sub
            raise Return
        else
            dp_array.[int row].[int col] <- 0
            __ret <- 0
            raise Return
        __ret
    with
        | Return -> __ret
let rec largest_square_area_in_matrix_top_down_with_dp (rows: int) (cols: int) (mat: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable rows = rows
    let mutable cols = cols
    let mutable mat = mat
    try
        let mutable largest: int array = unbox<int array> [|0|]
        let mutable dp_array: int array array = Array.empty<int array>
        let mutable r: int = 0
        while r < rows do
            let mutable row_list: int array = Array.empty<int>
            let mutable c: int = 0
            while c < cols do
                row_list <- Array.append row_list [|(-1)|]
                c <- c + 1
            dp_array <- Array.append dp_array [|row_list|]
            r <- r + 1
        update_area_of_max_square_with_dp (0) (0) (rows) (cols) (mat) (dp_array) (largest)
        __ret <- _idx largest (int 0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec largest_square_area_in_matrix_bottom_up (rows: int) (cols: int) (mat: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable rows = rows
    let mutable cols = cols
    let mutable mat = mat
    try
        let mutable dp_array: int array array = Array.empty<int array>
        let mutable r: int = 0
        while r <= rows do
            let mutable row_list: int array = Array.empty<int>
            let mutable c: int = 0
            while c <= cols do
                row_list <- Array.append row_list [|0|]
                c <- c + 1
            dp_array <- Array.append dp_array [|row_list|]
            r <- r + 1
        let mutable largest: int = 0
        let mutable row: int = rows - 1
        while row >= 0 do
            let mutable col: int = cols - 1
            while col >= 0 do
                let right: int = _idx (_idx dp_array (int row)) (int (col + 1))
                let diagonal: int = _idx (_idx dp_array (int (row + 1))) (int (col + 1))
                let bottom: int = _idx (_idx dp_array (int (row + 1))) (int col)
                if (_idx (_idx mat (int row)) (int col)) = 1 then
                    let value = 1 + (int (Array.min ([|right; diagonal; bottom|])))
                    dp_array.[int row].[int col] <- value
                    if (int value) > largest then
                        largest <- int value
                else
                    dp_array.[int row].[int col] <- 0
                col <- col - 1
            row <- row - 1
        __ret <- largest
        raise Return
        __ret
    with
        | Return -> __ret
let rec largest_square_area_in_matrix_bottom_up_space_optimization (rows: int) (cols: int) (mat: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable rows = rows
    let mutable cols = cols
    let mutable mat = mat
    try
        let mutable current_row: int array = Array.empty<int>
        let mutable i: int = 0
        while i <= cols do
            current_row <- Array.append current_row [|0|]
            i <- i + 1
        let mutable next_row: int array = Array.empty<int>
        let mutable j: int = 0
        while j <= cols do
            next_row <- Array.append next_row [|0|]
            j <- j + 1
        let mutable largest: int = 0
        let mutable row: int = rows - 1
        while row >= 0 do
            let mutable col: int = cols - 1
            while col >= 0 do
                let right: int = _idx current_row (int (col + 1))
                let diagonal: int = _idx next_row (int (col + 1))
                let bottom: int = _idx next_row (int col)
                if (_idx (_idx mat (int row)) (int col)) = 1 then
                    let value = 1 + (int (Array.min ([|right; diagonal; bottom|])))
                    current_row.[int col] <- value
                    if (int value) > largest then
                        largest <- int value
                else
                    current_row.[int col] <- 0
                col <- col - 1
            next_row <- current_row
            current_row <- Array.empty<int>
            let mutable t: int = 0
            while t <= cols do
                current_row <- Array.append current_row [|0|]
                t <- t + 1
            row <- row - 1
        __ret <- largest
        raise Return
        __ret
    with
        | Return -> __ret
let sample: int array array = [|[|1; 1|]; [|1; 1|]|]
printfn "%d" (largest_square_area_in_matrix_top_down (2) (2) (sample))
printfn "%d" (largest_square_area_in_matrix_top_down_with_dp (2) (2) (sample))
printfn "%d" (largest_square_area_in_matrix_bottom_up (2) (2) (sample))
printfn "%d" (largest_square_area_in_matrix_bottom_up_space_optimization (2) (2) (sample))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
