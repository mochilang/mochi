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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec mean_threshold (image: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable image = image
    try
        let height: int = Seq.length (image)
        let width: int = Seq.length (_idx image (0))
        let mutable total: int = 0
        let mutable i: int = 0
        while i < height do
            let mutable j: int = 0
            while j < width do
                total <- total + (_idx (_idx image (i)) (j))
                j <- j + 1
            i <- i + 1
        let mean: int = total / (height * width)
        i <- 0
        while i < height do
            let mutable j: int = 0
            while j < width do
                if (_idx (_idx image (i)) (j)) > mean then
                    image.[i].[j] <- 255
                else
                    image.[i].[j] <- 0
                j <- j + 1
            i <- i + 1
        __ret <- image
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_image (image: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable image = image
    try
        let mutable i: int = 0
        while i < (Seq.length (image)) do
            printfn "%s" (_repr (_idx image (i)))
            i <- i + 1
        __ret
    with
        | Return -> __ret
let img: int array array = [|[|10; 200; 50|]; [|100; 150; 30|]; [|90; 80; 220|]|]
let result: int array array = mean_threshold (img)
print_image (result)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
