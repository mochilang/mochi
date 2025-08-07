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
let PI: float = 3.141592653589793
let rec expApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
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
let rec gen_gaussian_kernel (k_size: int) (sigma: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable k_size = k_size
    let mutable sigma = sigma
    try
        let center: int = k_size / 2
        let mutable kernel: float array array = [||]
        let mutable i: int = 0
        while i < k_size do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < k_size do
                let x: float = float (i - center)
                let y: float = float (j - center)
                let exponent: float = -(((x * x) + (y * y)) / ((2.0 * sigma) * sigma))
                let value: float = (1.0 / ((2.0 * PI) * sigma)) * (expApprox (exponent))
                row <- Array.append row [|value|]
                j <- j + 1
            kernel <- Array.append kernel [|row|]
            i <- i + 1
        __ret <- kernel
        raise Return
        __ret
    with
        | Return -> __ret
let rec gaussian_filter (image: int array array) (k_size: int) (sigma: float) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable image = image
    let mutable k_size = k_size
    let mutable sigma = sigma
    try
        let height: int = Seq.length (image)
        let width: int = Seq.length (_idx image (0))
        let dst_height: int = (height - k_size) + 1
        let dst_width: int = (width - k_size) + 1
        let mutable kernel: float array array = gen_gaussian_kernel (k_size) (sigma)
        let mutable dst: int array array = [||]
        let mutable i: int = 0
        while i < dst_height do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < dst_width do
                let mutable sum: float = 0.0
                let mutable ki: int = 0
                while ki < k_size do
                    let mutable kj: int = 0
                    while kj < k_size do
                        sum <- sum + ((float (_idx (_idx image (i + ki)) (j + kj))) * (_idx (_idx kernel (ki)) (kj)))
                        kj <- kj + 1
                    ki <- ki + 1
                row <- Array.append row [|int sum|]
                j <- j + 1
            dst <- Array.append dst [|row|]
            i <- i + 1
        __ret <- dst
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
let img: int array array = [|[|52; 55; 61; 59; 79|]; [|62; 59; 55; 104; 94|]; [|63; 65; 66; 113; 144|]; [|68; 70; 70; 126; 154|]; [|70; 72; 69; 128; 155|]|]
let gaussian3: int array array = gaussian_filter (img) (3) (1.0)
let gaussian5: int array array = gaussian_filter (img) (5) (0.8)
print_image (gaussian3)
print_image (gaussian5)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
