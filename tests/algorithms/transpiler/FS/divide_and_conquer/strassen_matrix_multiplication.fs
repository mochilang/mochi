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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let rec default_matrix_multiplication (a: int array array) (b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable a = a
    let mutable b = b
    try
        __ret <- [|[|((_idx (_idx a (0)) (0)) * (_idx (_idx b (0)) (0))) + ((_idx (_idx a (0)) (1)) * (_idx (_idx b (1)) (0))); ((_idx (_idx a (0)) (0)) * (_idx (_idx b (0)) (1))) + ((_idx (_idx a (0)) (1)) * (_idx (_idx b (1)) (1)))|]; [|((_idx (_idx a (1)) (0)) * (_idx (_idx b (0)) (0))) + ((_idx (_idx a (1)) (1)) * (_idx (_idx b (1)) (0))); ((_idx (_idx a (1)) (0)) * (_idx (_idx b (0)) (1))) + ((_idx (_idx a (1)) (1)) * (_idx (_idx b (1)) (1)))|]|]
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_addition (matrix_a: int array array) (matrix_b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable matrix_a = matrix_a
    let mutable matrix_b = matrix_b
    try
        let mutable result: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (matrix_a)) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx matrix_a (i))) do
                row <- Array.append row [|(_idx (_idx matrix_a (i)) (j)) + (_idx (_idx matrix_b (i)) (j))|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_subtraction (matrix_a: int array array) (matrix_b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable matrix_a = matrix_a
    let mutable matrix_b = matrix_b
    try
        let mutable result: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (matrix_a)) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx matrix_a (i))) do
                row <- Array.append row [|(_idx (_idx matrix_a (i)) (j)) - (_idx (_idx matrix_b (i)) (j))|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and split_matrix (a: int array array) =
    let mutable __ret : int array array array = Unchecked.defaultof<int array array array>
    let mutable a = a
    try
        let n: int = Seq.length (a)
        let mid: int = n / 2
        let mutable top_left: int array array = [||]
        let mutable top_right: int array array = [||]
        let mutable bot_left: int array array = [||]
        let mutable bot_right: int array array = [||]
        let mutable i: int = 0
        while i < mid do
            let mutable left_row: int array = [||]
            let mutable right_row: int array = [||]
            let mutable j: int = 0
            while j < mid do
                left_row <- Array.append left_row [|_idx (_idx a (i)) (j)|]
                right_row <- Array.append right_row [|_idx (_idx a (i)) (j + mid)|]
                j <- j + 1
            top_left <- Array.append top_left [|left_row|]
            top_right <- Array.append top_right [|right_row|]
            i <- i + 1
        i <- mid
        while i < n do
            let mutable left_row: int array = [||]
            let mutable right_row: int array = [||]
            let mutable j: int = 0
            while j < mid do
                left_row <- Array.append left_row [|_idx (_idx a (i)) (j)|]
                right_row <- Array.append right_row [|_idx (_idx a (i)) (j + mid)|]
                j <- j + 1
            bot_left <- Array.append bot_left [|left_row|]
            bot_right <- Array.append bot_right [|right_row|]
            i <- i + 1
        __ret <- [|top_left; top_right; bot_left; bot_right|]
        raise Return
        __ret
    with
        | Return -> __ret
and matrix_dimensions (matrix: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable matrix = matrix
    try
        __ret <- unbox<int array> [|Seq.length (matrix); Seq.length (_idx matrix (0))|]
        raise Return
        __ret
    with
        | Return -> __ret
and next_power_of_two (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable p: int = 1
        while p < n do
            p <- p * 2
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and pad_matrix (mat: int array array) (rows: int) (cols: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable mat = mat
    let mutable rows = rows
    let mutable cols = cols
    try
        let mutable res: int array array = [||]
        let mutable i: int = 0
        while i < rows do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < cols do
                let mutable v: int = 0
                if (i < (Seq.length (mat))) && (j < (Seq.length (_idx mat (0)))) then
                    v <- _idx (_idx mat (i)) (j)
                row <- Array.append row [|v|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and actual_strassen (matrix_a: int array array) (matrix_b: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable matrix_a = matrix_a
    let mutable matrix_b = matrix_b
    try
        if (_idx (matrix_dimensions (matrix_a)) (0)) = 2 then
            __ret <- default_matrix_multiplication (matrix_a) (matrix_b)
            raise Return
        let parts_a: int array array array = split_matrix (matrix_a)
        let a: int array array = _idx parts_a (0)
        let b: int array array = _idx parts_a (1)
        let c: int array array = _idx parts_a (2)
        let d: int array array = _idx parts_a (3)
        let parts_b: int array array array = split_matrix (matrix_b)
        let e: int array array = _idx parts_b (0)
        let f: int array array = _idx parts_b (1)
        let g: int array array = _idx parts_b (2)
        let h: int array array = _idx parts_b (3)
        let t1: int array array = actual_strassen (a) (matrix_subtraction (f) (h))
        let t2: int array array = actual_strassen (matrix_addition (a) (b)) (h)
        let t3: int array array = actual_strassen (matrix_addition (c) (d)) (e)
        let t4: int array array = actual_strassen (d) (matrix_subtraction (g) (e))
        let t5: int array array = actual_strassen (matrix_addition (a) (d)) (matrix_addition (e) (h))
        let t6: int array array = actual_strassen (matrix_subtraction (b) (d)) (matrix_addition (g) (h))
        let t7: int array array = actual_strassen (matrix_subtraction (a) (c)) (matrix_addition (e) (f))
        let mutable top_left: int array array = matrix_addition (matrix_subtraction (matrix_addition (t5) (t4)) (t2)) (t6)
        let mutable top_right: int array array = matrix_addition (t1) (t2)
        let mutable bot_left: int array array = matrix_addition (t3) (t4)
        let mutable bot_right: int array array = matrix_subtraction (matrix_subtraction (matrix_addition (t1) (t5)) (t3)) (t7)
        let mutable new_matrix: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (top_right)) do
            new_matrix <- Array.append new_matrix [|unbox<int array> (Array.append (_idx top_left (i)) (_idx top_right (i)))|]
            i <- i + 1
        i <- 0
        while i < (Seq.length (bot_right)) do
            new_matrix <- Array.append new_matrix [|unbox<int array> (Array.append (_idx bot_left (i)) (_idx bot_right (i)))|]
            i <- i + 1
        __ret <- new_matrix
        raise Return
        __ret
    with
        | Return -> __ret
and strassen (matrix1: int array array) (matrix2: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable matrix1 = matrix1
    let mutable matrix2 = matrix2
    try
        let dims1: int array = matrix_dimensions (matrix1)
        let dims2: int array = matrix_dimensions (matrix2)
        if (_idx dims1 (1)) <> (_idx dims2 (0)) then
            __ret <- Array.empty<int array>
            raise Return
        let maximum: int = int (Array.max ([|_idx dims1 (0); _idx dims1 (1); _idx dims2 (0); _idx dims2 (1)|]))
        let size: int = next_power_of_two (maximum)
        let new_matrix1: int array array = pad_matrix (matrix1) (size) (size)
        let new_matrix2: int array array = pad_matrix (matrix2) (size) (size)
        let result_padded: int array array = actual_strassen (new_matrix1) (new_matrix2)
        let mutable final_matrix: int array array = [||]
        let mutable i: int = 0
        while i < (_idx dims1 (0)) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (_idx dims2 (1)) do
                row <- Array.append row [|_idx (_idx result_padded (i)) (j)|]
                j <- j + 1
            final_matrix <- Array.append final_matrix [|row|]
            i <- i + 1
        __ret <- final_matrix
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let matrix1: int array array = [|[|2; 3; 4; 5|]; [|6; 4; 3; 1|]; [|2; 3; 6; 7|]; [|3; 1; 2; 4|]; [|2; 3; 4; 5|]; [|6; 4; 3; 1|]; [|2; 3; 6; 7|]; [|3; 1; 2; 4|]; [|2; 3; 4; 5|]; [|6; 2; 3; 1|]|]
        let matrix2: int array array = [|[|0; 2; 1; 1|]; [|16; 2; 3; 3|]; [|2; 2; 7; 7|]; [|13; 11; 22; 4|]|]
        let mutable res: int array array = strassen (matrix1) (matrix2)
        printfn "%s" (_repr (res))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
