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
let rec get_neighbors_pixel (image: int array array) (x: int) (y: int) (center: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable image = image
    let mutable x = x
    let mutable y = y
    let mutable center = center
    try
        if (x < 0) || (y < 0) then
            __ret <- 0
            raise Return
        if (x >= (Seq.length (image))) || (y >= (Seq.length (_idx image (0)))) then
            __ret <- 0
            raise Return
        if (_idx (_idx image (x)) (y)) >= center then
            __ret <- 1
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
let rec local_binary_value (image: int array array) (x: int) (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable image = image
    let mutable x = x
    let mutable y = y
    try
        let center: int = _idx (_idx image (x)) (y)
        let powers: int array = [|1; 2; 4; 8; 16; 32; 64; 128|]
        let neighbors: int array = [|get_neighbors_pixel (image) (x - 1) (y + 1) (center); get_neighbors_pixel (image) (x) (y + 1) (center); get_neighbors_pixel (image) (x - 1) (y) (center); get_neighbors_pixel (image) (x + 1) (y + 1) (center); get_neighbors_pixel (image) (x + 1) (y) (center); get_neighbors_pixel (image) (x + 1) (y - 1) (center); get_neighbors_pixel (image) (x) (y - 1) (center); get_neighbors_pixel (image) (x - 1) (y - 1) (center)|]
        let mutable sum: int = 0
        let mutable i: int = 0
        while i < (Seq.length (neighbors)) do
            sum <- sum + ((_idx neighbors (i)) * (_idx powers (i)))
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let image: int array array = [|[|10; 10; 10; 10; 10|]; [|10; 20; 30; 20; 10|]; [|10; 30; 40; 30; 10|]; [|10; 20; 30; 20; 10|]; [|10; 10; 10; 10; 10|]|]
let mutable i: int = 0
while i < (Seq.length (image)) do
    let mutable j: int = 0
    let mutable line: string = ""
    while j < (Seq.length (_idx image (0))) do
        let value: int = local_binary_value (image) (i) (j)
        line <- line + (_str (value))
        if j < ((Seq.length (_idx image (0))) - 1) then
            line <- line + " "
        j <- j + 1
    printfn "%s" (line)
    i <- i + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
