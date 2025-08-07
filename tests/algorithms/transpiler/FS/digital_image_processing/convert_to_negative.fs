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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let rec convert_to_negative (img: int array array array) =
    let mutable __ret : int array array array = Unchecked.defaultof<int array array array>
    let mutable img = img
    try
        let mutable result: int array array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (img)) do
            let mutable row: int array array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx img (i))) do
                let pixel: int array = _idx (_idx img (i)) (j)
                let r: int = 255 - (_idx pixel (0))
                let g: int = 255 - (_idx pixel (1))
                let b: int = 255 - (_idx pixel (2))
                row <- Array.append row [|[|r; g; b|]|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let image: int array array array = [|[|[|10; 20; 30|]; [|0; 0; 0|]|]; [|[|255; 255; 255|]; [|100; 150; 200|]|]|]
        let neg: int array array array = convert_to_negative (image)
        printfn "%s" (_repr (neg))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
