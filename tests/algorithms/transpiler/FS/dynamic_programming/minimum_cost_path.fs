// Generated 2025-08-13 07:12 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec min_int (a: int) (b: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and minimum_cost_path (matrix: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable matrix = matrix
    try
        let rows: int = Seq.length (matrix)
        let cols: int = Seq.length (_idx matrix (int 0))
        let mutable j: int = 1
        while j < cols do
            let mutable row0: int array = _idx matrix (int 0)
            row0.[j] <- (_idx row0 (int j)) + (_idx row0 (int (j - 1)))
            matrix.[0] <- row0
            j <- j + 1
        let mutable i: int = 1
        while i < rows do
            let mutable row: int array = _idx matrix (int i)
            row.[0] <- (_idx row (int 0)) + (_idx (_idx matrix (int (i - 1))) (int 0))
            matrix.[i] <- row
            i <- i + 1
        i <- 1
        while i < rows do
            let mutable row: int array = _idx matrix (int i)
            j <- 1
            while j < cols do
                let up: int = _idx (_idx matrix (int (i - 1))) (int j)
                let left: int = _idx row (int (j - 1))
                let best: int = min_int (up) (left)
                row.[j] <- (_idx row (int j)) + best
                j <- j + 1
            matrix.[i] <- row
            i <- i + 1
        __ret <- _idx (_idx matrix (int (rows - 1))) (int (cols - 1))
        raise Return
        __ret
    with
        | Return -> __ret
let mutable m1: int array array = [|[|2; 1|]; [|3; 1|]; [|4; 2|]|]
let mutable m2: int array array = [|[|2; 1; 4|]; [|2; 1; 3|]; [|3; 2; 1|]|]
ignore (printfn "%s" (_str (minimum_cost_path (m1))))
ignore (printfn "%s" (_str (minimum_cost_path (m2))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
