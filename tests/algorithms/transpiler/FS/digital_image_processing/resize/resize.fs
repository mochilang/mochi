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
let rec zeros3d (h: int) (w: int) (c: int) =
    let mutable __ret : int array array array = Unchecked.defaultof<int array array array>
    let mutable h = h
    let mutable w = w
    let mutable c = c
    try
        let mutable arr: int array array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: int array array = [||]
            let mutable x: int = 0
            while x < w do
                let mutable pixel: int array = [||]
                let mutable k: int = 0
                while k < c do
                    pixel <- Array.append pixel [|0|]
                    k <- k + 1
                row <- Array.append row [|pixel|]
                x <- x + 1
            arr <- Array.append arr [|row|]
            y <- y + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
and resize_nn (img: int array array array) (dst_w: int) (dst_h: int) =
    let mutable __ret : int array array array = Unchecked.defaultof<int array array array>
    let mutable img = img
    let mutable dst_w = dst_w
    let mutable dst_h = dst_h
    try
        let src_h: int = Seq.length (img)
        let src_w: int = Seq.length (_idx img (0))
        let channels: int = Seq.length (_idx (_idx img (0)) (0))
        let ratio_x: float = (float src_w) / (float dst_w)
        let ratio_y: float = (float src_h) / (float dst_h)
        let mutable out: int array array array = zeros3d (dst_h) (dst_w) (channels)
        let mutable i: int = 0
        while i < dst_h do
            let mutable j: int = 0
            while j < dst_w do
                let src_x: int = int (ratio_x * (float j))
                let src_y: int = int (ratio_y * (float i))
                out.[i].[j] <- _idx (_idx img (src_y)) (src_x)
                j <- j + 1
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let img: int array array array = [|[|[|0; 0; 0|]; [|255; 255; 255|]|]; [|[|255; 0; 0|]; [|0; 255; 0|]|]|]
        let resized: int array array array = resize_nn (img) (4) (4)
        printfn "%s" (_repr (resized))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
