// Generated 2025-08-07 10:31 +0700

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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec iterator_values (matrix: int array array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable matrix = matrix
    try
        let mutable result: int array = [||]
        for row in matrix do
            for value in row do
                result <- Array.append result [|value|]
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec index_2d_array_in_1d (array: int array array) (index: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable array = array
    let mutable index = index
    try
        let rows: int = Seq.length (array)
        let cols: int = Seq.length (_idx array (0))
        if (rows = 0) || (cols = 0) then
            failwith ("no items in array")
        if (index < 0) || (index >= (rows * cols)) then
            failwith ("index out of range")
        __ret <- _idx (_idx array (int (index / cols))) (((index % cols + cols) % cols))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (iterator_values ([|[|5|]; [|-523|]; [|-1|]; [|34|]; [|0|]|])))
printfn "%s" (_str (iterator_values ([|[|5; -523; -1|]; [|34; 0|]|])))
printfn "%s" (_str (index_2d_array_in_1d ([|[|0; 1; 2; 3|]; [|4; 5; 6; 7|]; [|8; 9; 10; 11|]|]) (5)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
