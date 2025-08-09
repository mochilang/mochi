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
open System.Collections.Generic

let rec encode (row: int) (col: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    let mutable col = col
    try
        __ret <- ((_str (row)) + ",") + (_str (col))
        raise Return
        __ret
    with
        | Return -> __ret
let rec is_safe (row: int) (col: int) (rows: int) (cols: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable row = row
    let mutable col = col
    let mutable rows = rows
    let mutable cols = cols
    try
        __ret <- (((row >= 0) && (row < rows)) && (col >= 0)) && (col < cols)
        raise Return
        __ret
    with
        | Return -> __ret
let rec has (seen: System.Collections.Generic.IDictionary<string, bool>) (key: string) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable seen = seen
    let mutable key = key
    try
        __ret <- seen.ContainsKey(key)
        raise Return
        __ret
    with
        | Return -> __ret
let rec depth_first_search (row: int) (col: int) (seen: System.Collections.Generic.IDictionary<string, bool>) (mat: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable row = row
    let mutable col = col
    let mutable seen = seen
    let mutable mat = mat
    try
        let rows: int = Seq.length (mat)
        let cols: int = Seq.length (_idx mat (int 0))
        let key: string = encode (row) (col)
        if ((is_safe (row) (col) (rows) (cols)) && (not (has (seen) (key)))) && ((_idx (_idx mat (int row)) (int col)) = 1) then
            seen.[key] <- true
            __ret <- (((1 + (depth_first_search (row + 1) (col) (seen) (mat))) + (depth_first_search (row - 1) (col) (seen) (mat))) + (depth_first_search (row) (col + 1) (seen) (mat))) + (depth_first_search (row) (col - 1) (seen) (mat))
            raise Return
        else
            __ret <- 0
            raise Return
        __ret
    with
        | Return -> __ret
let rec find_max_area (mat: int array array) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable mat = mat
    try
        let mutable seen: System.Collections.Generic.IDictionary<string, bool> = _dictCreate []
        let rows: int = Seq.length (mat)
        let mutable max_area: int = 0
        let mutable r: int = 0
        while r < rows do
            let line: int array = _idx mat (int r)
            let cols: int = Seq.length (line)
            let mutable c: int = 0
            while c < cols do
                if (_idx line (int c)) = 1 then
                    let key: string = encode (r) (c)
                    if not (seen.ContainsKey(key)) then
                        let area: int = depth_first_search (r) (c) (seen) (mat)
                        if area > max_area then
                            max_area <- area
                c <- c + 1
            r <- r + 1
        __ret <- max_area
        raise Return
        __ret
    with
        | Return -> __ret
let matrix: int array array = [|[|0; 0; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0|]; [|0; 1; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0|]; [|0; 1; 0; 0; 1; 1; 0; 0; 1; 0; 1; 0; 0|]; [|0; 1; 0; 0; 1; 1; 0; 0; 1; 1; 1; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0|]|]
printfn "%d" (find_max_area (matrix))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
