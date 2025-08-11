// Generated 2025-08-11 16:20 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec solution (length: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable length = length
    try
        let mutable ways: int array array = Array.empty<int array>
        let mutable i: int = 0
        while i <= length do
            let mutable row: int array = Array.empty<int>
            row <- Array.append row [|0|]
            row <- Array.append row [|0|]
            row <- Array.append row [|0|]
            ways <- Array.append ways [|row|]
            i <- i + 1
        let mutable row_length: int = 0
        while row_length <= length do
            let mutable tile_length: int = 2
            while tile_length <= 4 do
                let mutable tile_start: int = 0
                while tile_start <= (row_length - tile_length) do
                    let remaining: int = (row_length - tile_start) - tile_length
                    ways.[int row_length].[int (tile_length - 2)] <- ((_idx (_idx ways (int row_length)) (int (tile_length - 2))) + (_idx (_idx ways (int remaining)) (int (tile_length - 2)))) + 1
                    tile_start <- tile_start + 1
                tile_length <- tile_length + 1
            row_length <- row_length + 1
        let mutable total: int = 0
        let mutable j: int = 0
        while j < 3 do
            total <- total + (_idx (_idx ways (int length)) (int j))
            j <- j + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (solution (5))
printfn "%d" (solution (50))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
