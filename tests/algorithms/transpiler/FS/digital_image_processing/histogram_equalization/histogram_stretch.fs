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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec make_list (n: int) (value: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable n = n
    let mutable value = value
    try
        let mutable res: int array = [||]
        let mutable i: int = 0
        while i < n do
            res <- Array.append res [|value|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec histogram_stretch (image: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable image = image
    try
        let height: int = Seq.length (image)
        let width: int = Seq.length (_idx image (0))
        let mutable hist: int array = make_list (256) (0)
        let mutable i: int = 0
        while i < height do
            let mutable j: int = 0
            while j < width do
                let ``val``: int = _idx (_idx image (i)) (j)
                hist.[``val``] <- (_idx hist (``val``)) + 1
                j <- j + 1
            i <- i + 1
        let mutable mapping: int array = make_list (256) (0)
        let mutable cumulative: int = 0
        let total: int = height * width
        let mutable h: int = 0
        while h < 256 do
            cumulative <- cumulative + (_idx hist (h))
            mapping.[h] <- (255 * cumulative) / total
            h <- h + 1
        i <- 0
        while i < height do
            let mutable j: int = 0
            while j < width do
                let ``val``: int = _idx (_idx image (i)) (j)
                image.[i].[j] <- _idx mapping (``val``)
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
let img: int array array = [|[|52; 55; 61|]; [|59; 79; 61|]; [|85; 76; 62|]|]
let result: int array array = histogram_stretch (img)
print_image (result)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
