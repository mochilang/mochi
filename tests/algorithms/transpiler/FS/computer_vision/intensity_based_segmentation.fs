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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let rec segment_image (image: int array array) (thresholds: int array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable image = image
    let mutable thresholds = thresholds
    try
        let mutable segmented: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (image)) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx image (i))) do
                let pixel: int = _idx (_idx image (i)) (j)
                let mutable label: int = 0
                let mutable k: int = 0
                while k < (Seq.length (thresholds)) do
                    if pixel > (_idx thresholds (k)) then
                        label <- k + 1
                    k <- k + 1
                row <- Array.append row [|label|]
                j <- j + 1
            segmented <- Array.append segmented [|row|]
            i <- i + 1
        __ret <- segmented
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let image: int array array = [|[|80; 120; 180|]; [|40; 90; 150|]; [|20; 60; 100|]|]
        let thresholds: int array = [|50; 100; 150|]
        let mutable segmented: int array array = segment_image (image) (thresholds)
        printfn "%s" (_repr (segmented))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
