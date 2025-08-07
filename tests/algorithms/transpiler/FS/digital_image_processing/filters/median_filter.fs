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
let rec insertion_sort (a: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable a = a
    try
        let mutable i: int = 1
        while i < (Seq.length (a)) do
            let key: int = _idx a (i)
            let mutable j: int = i - 1
            while (j >= 0) && ((_idx a (j)) > key) do
                a.[j + 1] <- _idx a (j)
                j <- j - 1
            a.[j + 1] <- key
            i <- i + 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
and median_filter (gray_img: int array array) (mask: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable gray_img = gray_img
    let mutable mask = mask
    try
        let rows: int = Seq.length (gray_img)
        let cols: int = Seq.length (_idx gray_img (0))
        let bd: int = mask / 2
        let mutable result: int array array = [||]
        let mutable i: int = 0
        while i < rows do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < cols do
                row <- Array.append row [|0|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        i <- bd
        while i < (rows - bd) do
            let mutable j: int = bd
            while j < (cols - bd) do
                let mutable kernel: int array = [||]
                let mutable x: int = i - bd
                while x <= (i + bd) do
                    let mutable y: int = j - bd
                    while y <= (j + bd) do
                        kernel <- Array.append kernel [|_idx (_idx gray_img (x)) (y)|]
                        y <- y + 1
                    x <- x + 1
                kernel <- insertion_sort (kernel)
                let idx: int = (mask * mask) / 2
                result.[i].[j] <- _idx kernel (idx)
                j <- j + 1
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
        let img: int array array = [|[|10; 10; 10; 10; 10|]; [|10; 255; 10; 255; 10|]; [|10; 10; 10; 10; 10|]; [|10; 255; 10; 255; 10|]; [|10; 10; 10; 10; 10|]|]
        let filtered: int array array = median_filter (img) (3)
        printfn "%s" (_repr (filtered))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
