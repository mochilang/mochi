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
let rec make_matrix (rows: int) (cols: int) (value: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable rows = rows
    let mutable cols = cols
    let mutable value = value
    try
        let mutable result: int array array = [||]
        let mutable i: int = 0
        while i < rows do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < cols do
                row <- Array.append row [|value|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec my_laplacian (src: int array array) (ksize: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable src = src
    let mutable ksize = ksize
    try
        let mutable kernel: int array array = [||]
        if ksize = 1 then
            kernel <- [|[|0; -1; 0|]; [|-1; 4; -1|]; [|0; -1; 0|]|]
        else
            if ksize = 3 then
                kernel <- [|[|0; 1; 0|]; [|1; -4; 1|]; [|0; 1; 0|]|]
            else
                if ksize = 5 then
                    kernel <- [|[|0; 0; -1; 0; 0|]; [|0; -1; -2; -1; 0|]; [|-1; -2; 16; -2; -1|]; [|0; -1; -2; -1; 0|]; [|0; 0; -1; 0; 0|]|]
                else
                    if ksize = 7 then
                        kernel <- [|[|0; 0; 0; -1; 0; 0; 0|]; [|0; 0; -2; -3; -2; 0; 0|]; [|0; -2; -7; -10; -7; -2; 0|]; [|-1; -3; -10; 68; -10; -3; -1|]; [|0; -2; -7; -10; -7; -2; 0|]; [|0; 0; -2; -3; -2; 0; 0|]; [|0; 0; 0; -1; 0; 0; 0|]|]
                    else
                        failwith ("ksize must be in (1, 3, 5, 7)")
        let rows: int = Seq.length (src)
        let cols: int = Seq.length (_idx src (0))
        let k: int = Seq.length (kernel)
        let pad: int = k / 2
        let mutable output: int array array = make_matrix (rows) (cols) (0)
        let mutable i: int = 0
        while i < rows do
            let mutable j: int = 0
            while j < cols do
                let mutable sum: int = 0
                let mutable ki: int = 0
                while ki < k do
                    let mutable kj: int = 0
                    while kj < k do
                        let ii: int = (i + ki) - pad
                        let jj: int = (j + kj) - pad
                        let mutable ``val``: int = 0
                        if (((ii >= 0) && (ii < rows)) && (jj >= 0)) && (jj < cols) then
                            ``val`` <- _idx (_idx src (ii)) (jj)
                        sum <- sum + (``val`` * (_idx (_idx kernel (ki)) (kj)))
                        kj <- kj + 1
                    ki <- ki + 1
                output.[i].[j] <- sum
                j <- j + 1
            i <- i + 1
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
let image: int array array = [|[|0; 0; 0; 0; 0|]; [|0; 10; 10; 10; 0|]; [|0; 10; 10; 10; 0|]; [|0; 10; 10; 10; 0|]; [|0; 0; 0; 0; 0|]|]
let mutable result: int array array = my_laplacian (image) (3)
let mutable r: int = 0
while r < (Seq.length (result)) do
    let mutable row_str: string = "["
    let mutable c: int = 0
    while c < (Seq.length (_idx result (r))) do
        row_str <- row_str + (_str (_idx (_idx result (r)) (c)))
        if (c + 1) < (Seq.length (_idx result (r))) then
            row_str <- row_str + ", "
        c <- c + 1
    row_str <- row_str + "]"
    printfn "%s" (row_str)
    r <- r + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
