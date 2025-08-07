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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs_int (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let e: float = 2.718281828
        let mutable n: int = 0
        let mutable y: float = x
        while y >= e do
            y <- y / e
            n <- n + 1
        while y <= (1.0 / e) do
            y <- y * e
            n <- n - 1
        y <- y - 1.0
        let mutable term: float = y
        let mutable result: float = 0.0
        let mutable k: int = 1
        while k <= 20 do
            if (((k % 2 + 2) % 2)) = 1 then
                result <- result + (term / (1.0 * (float k)))
            else
                result <- result - (term / (1.0 * (float k)))
            term <- term * y
            k <- k + 1
        __ret <- result + (1.0 * (float n))
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_concurrency (image: int array array) (coord: int array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable image = image
    let mutable coord = coord
    try
        let offset_x: int = _idx coord (0)
        let offset_y: int = _idx coord (1)
        let mutable max_val: int = 0
        for r in 0 .. ((Seq.length (image)) - 1) do
            for c in 0 .. ((Seq.length (_idx image (r))) - 1) do
                if (_idx (_idx image (r)) (c)) > max_val then
                    max_val <- _idx (_idx image (r)) (c)
        let size: int = max_val + 1
        let mutable matrix: float array array = [||]
        for i in 0 .. (size - 1) do
            let mutable row: float array = [||]
            for j in 0 .. (size - 1) do
                row <- Array.append row [|0.0|]
            matrix <- Array.append matrix [|row|]
        for x in 1 .. (((Seq.length (image)) - 1) - 1) do
            for y in 1 .. (((Seq.length (_idx image (x))) - 1) - 1) do
                let ``base``: int = _idx (_idx image (x)) (y)
                let offset: int = _idx (_idx image (x + offset_x)) (y + offset_y)
                matrix.[``base``].[offset] <- (_idx (_idx matrix (``base``)) (offset)) + 1.0
        let mutable total: float = 0.0
        for i in 0 .. (size - 1) do
            for j in 0 .. (size - 1) do
                total <- total + (_idx (_idx matrix (i)) (j))
        if total = 0.0 then
            __ret <- matrix
            raise Return
        for i in 0 .. (size - 1) do
            for j in 0 .. (size - 1) do
                matrix.[i].[j] <- (_idx (_idx matrix (i)) (j)) / total
        __ret <- matrix
        raise Return
        __ret
    with
        | Return -> __ret
let rec haralick_descriptors (matrix: float array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable matrix = matrix
    try
        let rows: int = Seq.length (matrix)
        let cols: int = Seq.length (_idx matrix (0))
        let mutable maximum_prob: float = 0.0
        let mutable correlation: float = 0.0
        let mutable energy: float = 0.0
        let mutable contrast: float = 0.0
        let mutable dissimilarity: float = 0.0
        let mutable inverse_difference: float = 0.0
        let mutable homogeneity: float = 0.0
        let mutable entropy: float = 0.0
        let mutable i: int = 0
        while i < rows do
            let mutable j: int = 0
            while j < cols do
                let ``val``: float = _idx (_idx matrix (i)) (j)
                if ``val`` > maximum_prob then
                    maximum_prob <- ``val``
                correlation <- correlation + (((1.0 * (float i)) * (float j)) * ``val``)
                energy <- energy + (``val`` * ``val``)
                let diff: int = i - j
                let adiff: int = abs_int (diff)
                contrast <- contrast + (``val`` * ((1.0 * (float diff)) * (float diff)))
                dissimilarity <- dissimilarity + (``val`` * (1.0 * (float adiff)))
                inverse_difference <- inverse_difference + (``val`` / (1.0 + (1.0 * (float adiff))))
                homogeneity <- homogeneity + (``val`` / (1.0 + ((1.0 * (float diff)) * (float diff))))
                if ``val`` > 0.0 then
                    entropy <- entropy - (``val`` * (ln (``val``)))
                j <- j + 1
            i <- i + 1
        __ret <- unbox<float array> [|maximum_prob; correlation; energy; contrast; dissimilarity; inverse_difference; homogeneity; entropy|]
        raise Return
        __ret
    with
        | Return -> __ret
let image: int array array = [|[|0; 1; 0|]; [|1; 0; 1|]; [|0; 1; 0|]|]
let glcm: float array array = matrix_concurrency (image) (unbox<int array> [|0; 1|])
let descriptors: float array = haralick_descriptors (glcm)
let mutable idx: int = 0
while idx < (Seq.length (descriptors)) do
    printfn "%s" (_str (_idx descriptors (idx)))
    idx <- idx + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
