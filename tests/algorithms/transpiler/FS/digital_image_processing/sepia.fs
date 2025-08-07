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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec normalize (value: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable value = value
    try
        __ret <- if value > 255 then 255 else value
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_grayscale (blue: int) (green: int) (red: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable blue = blue
    let mutable green = green
    let mutable red = red
    try
        let gs: float = ((0.2126 * (float red)) + (0.587 * (float green))) + (0.114 * (float blue))
        __ret <- int (gs)
        raise Return
        __ret
    with
        | Return -> __ret
let rec make_sepia (img: int array array array) (factor: int) =
    let mutable __ret : int array array array = Unchecked.defaultof<int array array array>
    let mutable img = img
    let mutable factor = factor
    try
        let pixel_h: int = Seq.length (img)
        let pixel_v: int = Seq.length (_idx img (0))
        let mutable i: int = 0
        while i < pixel_h do
            let mutable j: int = 0
            while j < pixel_v do
                let pixel: int array = _idx (_idx img (i)) (j)
                let grey: int = to_grayscale (_idx pixel (0)) (_idx pixel (1)) (_idx pixel (2))
                img.[i].[j] <- [|normalize (grey); normalize (grey + factor); normalize (grey + (2 * factor))|]
                j <- j + 1
            i <- i + 1
        __ret <- img
        raise Return
        __ret
    with
        | Return -> __ret
let mutable image: int array array array = [|[|[|10; 20; 30|]; [|40; 50; 60|]|]; [|[|70; 80; 90|]; [|200; 150; 100|]|]|]
let sepia: int array array array = make_sepia (image) (20)
printfn "%s" (_str (sepia))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
