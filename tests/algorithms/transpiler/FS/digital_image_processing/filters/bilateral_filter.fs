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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (x: float) =
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
let rec expApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n < 10 do
            term <- (term * x) / (float n)
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec vec_gaussian (mat: float array array) (variance: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    let mutable variance = variance
    try
        let mutable i: int = 0
        let mutable out: float array array = [||]
        while i < (Seq.length (mat)) do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (i))) do
                let v: float = _idx (_idx mat (i)) (j)
                let e: float = (-(v * v)) / (2.0 * variance)
                row <- unbox<float array> (Array.append row [|expApprox (e)|])
                j <- j + 1
            out <- unbox<float array array> (Array.append out [|row|])
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_slice (img: float array array) (x: int) (y: int) (kernel_size: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable img = img
    let mutable x = x
    let mutable y = y
    let mutable kernel_size = kernel_size
    try
        let half: int = kernel_size / 2
        let mutable i: int = x - half
        let mutable slice: float array array = [||]
        while i <= (x + half) do
            let mutable row: float array = [||]
            let mutable j: int = y - half
            while j <= (y + half) do
                row <- unbox<float array> (Array.append row [|_idx (_idx img (i)) (j)|])
                j <- j + 1
            slice <- unbox<float array array> (Array.append slice [|row|])
            i <- i + 1
        __ret <- slice
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_gauss_kernel (kernel_size: int) (spatial_variance: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable kernel_size = kernel_size
    let mutable spatial_variance = spatial_variance
    try
        let mutable arr: float array array = [||]
        let mutable i: int = 0
        while i < kernel_size do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < kernel_size do
                let di: float = float (i - (kernel_size / 2))
                let dj: float = float (j - (kernel_size / 2))
                let dist: float = sqrtApprox ((di * di) + (dj * dj))
                row <- unbox<float array> (Array.append row [|dist|])
                j <- j + 1
            arr <- unbox<float array array> (Array.append arr [|row|])
            i <- i + 1
        __ret <- vec_gaussian (arr) (spatial_variance)
        raise Return
        __ret
    with
        | Return -> __ret
let rec elementwise_sub (mat: float array array) (value: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    let mutable value = value
    try
        let mutable res: float array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (i))) do
                row <- unbox<float array> (Array.append row [|(_idx (_idx mat (i)) (j)) - value|])
                j <- j + 1
            res <- unbox<float array array> (Array.append res [|row|])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec elementwise_mul (a: float array array) (b: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable a = a
    let mutable b = b
    try
        let mutable res: float array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (a)) do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx a (i))) do
                row <- unbox<float array> (Array.append row [|(_idx (_idx a (i)) (j)) * (_idx (_idx b (i)) (j))|])
                j <- j + 1
            res <- unbox<float array array> (Array.append res [|row|])
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec matrix_sum (mat: float array array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable mat = mat
    try
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (i))) do
                total <- total + (_idx (_idx mat (i)) (j))
                j <- j + 1
            i <- i + 1
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
let rec bilateral_filter (img: float array array) (spatial_variance: float) (intensity_variance: float) (kernel_size: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable img = img
    let mutable spatial_variance = spatial_variance
    let mutable intensity_variance = intensity_variance
    let mutable kernel_size = kernel_size
    try
        let gauss_ker: float array array = get_gauss_kernel (kernel_size) (spatial_variance)
        let img_s: float array array = img
        let center: float = _idx (_idx img_s (kernel_size / 2)) (kernel_size / 2)
        let img_i: float array array = elementwise_sub (img_s) (center)
        let img_ig: float array array = vec_gaussian (img_i) (intensity_variance)
        let weights: float array array = elementwise_mul (gauss_ker) (img_ig)
        let vals: float array array = elementwise_mul (img_s) (weights)
        let sum_weights: float = matrix_sum (weights)
        let mutable ``val``: float = 0.0
        if sum_weights <> 0.0 then
            ``val`` <- (matrix_sum (vals)) / sum_weights
        __ret <- ``val``
        raise Return
        __ret
    with
        | Return -> __ret
let img: float array array = [|[|0.2; 0.3; 0.4|]; [|0.3; 0.4; 0.5|]; [|0.4; 0.5; 0.6|]|]
let result: float = bilateral_filter (img) (1.0) (1.0) (3)
printfn "%g" (result)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
