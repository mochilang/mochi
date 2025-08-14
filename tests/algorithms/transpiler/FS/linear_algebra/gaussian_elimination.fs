// Generated 2025-08-14 17:48 +0700

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
let rec retroactive_resolution (coefficients: float array array) (vector: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable coefficients = coefficients
    let mutable vector = vector
    try
        let rows: int = Seq.length (coefficients)
        let mutable x: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rows do
            let mutable inner: float array = Array.empty<float>
            inner <- Array.append inner [|0.0|]
            x <- Array.append x [|inner|]
            i <- i + 1
        let mutable r: int = rows - 1
        while r >= 0 do
            let mutable total: float = 0.0
            let mutable c: int = r + 1
            while c < rows do
                total <- total + ((_idx (_idx coefficients (int r)) (int c)) * (_idx (_idx x (int c)) (int 0)))
                c <- c + 1
            x.[r].[0] <- ((_idx (_idx vector (int r)) (int 0)) - total) / (_idx (_idx coefficients (int r)) (int r))
            r <- r - 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and gaussian_elimination (coefficients: float array array) (vector: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable coefficients = coefficients
    let mutable vector = vector
    try
        let rows: int = Seq.length (coefficients)
        let columns: int = Seq.length (_idx coefficients (int 0))
        if rows <> columns then
            __ret <- Array.empty<float array>
            raise Return
        let mutable augmented: float array array = Array.empty<float array>
        let mutable i: int = 0
        while i < rows do
            let mutable row: float array = Array.empty<float>
            let mutable j: int = 0
            while j < columns do
                row <- Array.append row [|(_idx (_idx coefficients (int i)) (int j))|]
                j <- j + 1
            row <- Array.append row [|(_idx (_idx vector (int i)) (int 0))|]
            augmented <- Array.append augmented [|row|]
            i <- i + 1
        let mutable row_idx: int = 0
        while row_idx < (rows - 1) do
            let pivot: float = _idx (_idx augmented (int row_idx)) (int row_idx)
            let mutable col: int = row_idx + 1
            while col < rows do
                let factor: float = (_idx (_idx augmented (int col)) (int row_idx)) / pivot
                let mutable k: int = row_idx
                while k < (columns + 1) do
                    augmented.[col].[k] <- (_idx (_idx augmented (int col)) (int k)) - (factor * (_idx (_idx augmented (int row_idx)) (int k)))
                    k <- k + 1
                col <- col + 1
            row_idx <- row_idx + 1
        let mutable coeffs: float array array = Array.empty<float array>
        let mutable vec: float array array = Array.empty<float array>
        let mutable r: int = 0
        while r < rows do
            let mutable row: float array = Array.empty<float>
            let mutable c: int = 0
            while c < columns do
                row <- Array.append row [|(_idx (_idx augmented (int r)) (int c))|]
                c <- c + 1
            coeffs <- Array.append coeffs [|row|]
            vec <- Array.append vec [|[|_idx (_idx augmented (int r)) (int columns)|]|]
            r <- r + 1
        let mutable x: float array array = retroactive_resolution (coeffs) (vec)
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%s" (_repr (gaussian_elimination ([|[|1.0; -4.0; -2.0|]; [|5.0; 2.0; -2.0|]; [|1.0; -1.0; 0.0|]|]) ([|[|-2.0|]; [|-3.0|]; [|4.0|]|]))))
ignore (printfn "%s" (_repr (gaussian_elimination ([|[|1.0; 2.0|]; [|5.0; 2.0|]|]) ([|[|5.0|]; [|5.0|]|]))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
