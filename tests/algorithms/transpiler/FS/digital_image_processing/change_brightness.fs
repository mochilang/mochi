// Generated 2025-08-07 14:57 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec clamp (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable value = value
    try
        if value < 0 then
            __ret <- 0
            raise Return
        if value > 255 then
            __ret <- 255
            raise Return
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let rec change_brightness (img: int array array) (level: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    let mutable level = level
    try
        if (level < (-255)) || (level > 255) then
            failwith ("level must be between -255 and 255")
        let mutable result: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (img)) do
            let mutable row_res: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx img (i))) do
                row_res <- Array.append row_res [|clamp ((_idx (_idx img (i)) (j)) + level)|]
                j <- j + 1
            result <- Array.append result [|row_res|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let sample: int array array = [|[|100; 150|]; [|200; 250|]|]
printfn "%s" (_repr (change_brightness (sample) (30)))
printfn "%s" (_repr (change_brightness (sample) (-60)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
