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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec conv2d (image: float array array) (kernel: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable image = image
    let mutable kernel = kernel
    try
        let rows: int = Seq.length (image)
        let cols: int = Seq.length (_idx image (0))
        let k: int = Seq.length (kernel)
        let mutable output: float array array = [||]
        let mutable i: int = 0
        while i <= (rows - k) do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j <= (cols - k) do
                let mutable sum: float = 0.0
                let mutable ki: int = 0
                while ki < k do
                    let mutable kj: int = 0
                    while kj < k do
                        sum <- sum + ((_idx (_idx image (i + ki)) (j + kj)) * (_idx (_idx kernel (ki)) (kj)))
                        kj <- kj + 1
                    ki <- ki + 1
                row <- Array.append row [|sum|]
                j <- j + 1
            output <- Array.append output [|row|]
            i <- i + 1
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
let rec relu_matrix (m: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable m = m
    try
        let mutable out: float array array = [||]
        for row in m do
            let mutable new_row: float array = [||]
            for v in row do
                if v > 0.0 then
                    new_row <- Array.append new_row [|v|]
                else
                    new_row <- Array.append new_row [|0.0|]
            out <- Array.append out [|new_row|]
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec max_pool2x2 (m: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable m = m
    try
        let rows: int = Seq.length (m)
        let cols: int = Seq.length (_idx m (0))
        let mutable out: float array array = [||]
        let mutable i: int = 0
        while i < rows do
            let mutable new_row: float array = [||]
            let mutable j: int = 0
            while j < cols do
                let mutable max_val: float = _idx (_idx m (i)) (j)
                if (_idx (_idx m (i)) (j + 1)) > max_val then
                    max_val <- _idx (_idx m (i)) (j + 1)
                if (_idx (_idx m (i + 1)) (j)) > max_val then
                    max_val <- _idx (_idx m (i + 1)) (j)
                if (_idx (_idx m (i + 1)) (j + 1)) > max_val then
                    max_val <- _idx (_idx m (i + 1)) (j + 1)
                new_row <- Array.append new_row [|max_val|]
                j <- j + 2
            out <- Array.append out [|new_row|]
            i <- i + 2
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec flatten (m: float array array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable m = m
    try
        let mutable res: float array = [||]
        for row in m do
            for v in row do
                res <- Array.append res [|v|]
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec dense (inputs: float array) (weights: float array) (bias: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable inputs = inputs
    let mutable weights = weights
    let mutable bias = bias
    try
        let mutable s: float = bias
        let mutable i: int = 0
        while i < (Seq.length (inputs)) do
            s <- s + ((_idx inputs (i)) * (_idx weights (i)))
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec exp_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
        let mutable i: int = 1
        while i <= 10 do
            term <- (term * x) / (float i)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec sigmoid (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- 1.0 / (1.0 + (exp_approx (-x)))
        raise Return
        __ret
    with
        | Return -> __ret
let image: float array array = [|[|0.0; 1.0; 1.0; 0.0; 0.0; 0.0|]; [|0.0; 1.0; 1.0; 0.0; 0.0; 0.0|]; [|0.0; 0.0; 1.0; 1.0; 0.0; 0.0|]; [|0.0; 0.0; 1.0; 1.0; 0.0; 0.0|]; [|0.0; 0.0; 0.0; 0.0; 0.0; 0.0|]; [|0.0; 0.0; 0.0; 0.0; 0.0; 0.0|]|]
let kernel: float array array = [|[|1.0; 0.0; -1.0|]; [|1.0; 0.0; -1.0|]; [|1.0; 0.0; -1.0|]|]
let conv: float array array = conv2d (image) (kernel)
let activated: float array array = relu_matrix (conv)
let pooled: float array array = max_pool2x2 (activated)
let flat: float array = flatten (pooled)
let weights: float array = [|0.5; -0.4; 0.3; 0.1|]
let bias: float = 0.0
let mutable output: float = dense (flat) (weights) (bias)
let probability: float = sigmoid (output)
if probability >= 0.5 then
    printfn "%s" ("Abnormality detected")
else
    printfn "%s" ("Normal")
printfn "%s" ("Probability:")
printfn "%g" (probability)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
