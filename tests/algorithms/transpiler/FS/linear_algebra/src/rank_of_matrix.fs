// Generated 2025-08-08 16:34 +0700

exception Break
exception Continue

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
let rec rank_of_matrix (matrix: float array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable matrix = matrix
    try
        let rows: int = Seq.length (matrix)
        if rows = 0 then
            __ret <- 0
            raise Return
        let columns: int = if (Seq.length (_idx matrix (0))) > 0 then (Seq.length (_idx matrix (0))) else 0
        let mutable rank: int = if rows < columns then rows else columns
        let mutable row: int = 0
        try
            while row < rank do
                try
                    if (_idx (_idx matrix (row)) (row)) <> 0.0 then
                        let mutable col: int = row + 1
                        while col < rows do
                            let mult: float = (_idx (_idx matrix (col)) (row)) / (_idx (_idx matrix (row)) (row))
                            let mutable i: int = row
                            while i < columns do
                                matrix.[col].[i] <- (_idx (_idx matrix (col)) (i)) - (mult * (_idx (_idx matrix (row)) (i)))
                                i <- i + 1
                            col <- col + 1
                    else
                        let mutable reduce: bool = true
                        let mutable i: int = row + 1
                        try
                            while i < rows do
                                try
                                    if (_idx (_idx matrix (i)) (row)) <> 0.0 then
                                        let temp: float array = _idx matrix (row)
                                        matrix.[row] <- _idx matrix (i)
                                        matrix.[i] <- temp
                                        reduce <- false
                                        raise Break
                                    i <- i + 1
                                with
                                | Continue -> ()
                                | Break -> raise Break
                        with
                        | Break -> ()
                        | Continue -> ()
                        if reduce then
                            rank <- rank - 1
                            let mutable j: int = 0
                            while j < rows do
                                matrix.[j].[row] <- _idx (_idx matrix (j)) (rank)
                                j <- j + 1
                        row <- row - 1
                    row <- row + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- rank
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
