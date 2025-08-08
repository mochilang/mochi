// Generated 2025-08-08 16:34 +0700

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
let rec panic (msg: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    try
        printfn "%s" (msg)
        __ret
    with
        | Return -> __ret
let rec abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec copy_matrix (src: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable src = src
    try
        let mutable res: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < (Seq.length (src)) do
            let mutable row_src: float array = _idx src (i)
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < (Seq.length (row_src)) do
                row <- Array.append row [|(_idx row_src (j))|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec solve_linear_system (matrix: float array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable matrix = matrix
    try
        let mutable ab: float array array = copy_matrix (matrix)
        let mutable num_rows: int = Seq.length (ab)
        let mutable num_cols: int = (Seq.length (_idx ab (0))) - 1
        if num_rows <> num_cols then
            panic ("Matrix is not square")
            __ret <- Array.empty<float>
            raise Return
        let mutable column_num: int = 0
        while column_num < num_rows do
            let mutable i: int = column_num
            while i < num_cols do
                if (abs_float (_idx (_idx ab (i)) (column_num))) > (abs_float (_idx (_idx ab (column_num)) (column_num))) then
                    let mutable temp: float array = _idx ab (column_num)
                    ab.[column_num] <- _idx ab (i)
                    ab.[i] <- temp
                i <- i + 1
            if (abs_float (_idx (_idx ab (column_num)) (column_num))) < 0.00000001 then
                panic ("Matrix is singular")
                __ret <- Array.empty<float>
                raise Return
            if column_num <> 0 then
                i <- column_num
                while i < num_rows do
                    let mutable factor: float = (_idx (_idx ab (i)) (column_num - 1)) / (_idx (_idx ab (column_num - 1)) (column_num - 1))
                    let mutable j: int = 0
                    while j < (Seq.length (_idx ab (i))) do
                        ab.[i].[j] <- (_idx (_idx ab (i)) (j)) - (factor * (_idx (_idx ab (column_num - 1)) (j)))
                        j <- j + 1
                    i <- i + 1
            column_num <- column_num + 1
        let mutable x_lst: float array = Array.empty<float>
        let mutable t: int = 0
        while t < num_rows do
            x_lst <- Array.append x_lst [|0.0|]
            t <- t + 1
        column_num <- num_rows - 1
        while column_num >= 0 do
            let mutable x: float = (_idx (_idx ab (column_num)) (num_cols)) / (_idx (_idx ab (column_num)) (column_num))
            x_lst.[column_num] <- x
            let mutable i: int = column_num - 1
            while i >= 0 do
                ab.[i].[num_cols] <- (_idx (_idx ab (i)) (num_cols)) - ((_idx (_idx ab (i)) (column_num)) * x)
                i <- i - 1
            column_num <- column_num - 1
        __ret <- x_lst
        raise Return
        __ret
    with
        | Return -> __ret
let mutable example_matrix: float array array = [|[|5.0; -5.0; -3.0; 4.0; -11.0|]; [|1.0; -4.0; 6.0; -4.0; -10.0|]; [|-2.0; -5.0; 4.0; -5.0; -12.0|]; [|-3.0; -3.0; 5.0; -5.0; 8.0|]|]
printfn "%s" ("Matrix:")
printfn "%s" (_str (example_matrix))
let mutable solution: float array = solve_linear_system (example_matrix)
printfn "%s" (_str (solution))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
