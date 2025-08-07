// Generated 2025-08-07 16:27 +0700

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
type Complex = {
    re: float
    im: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec complex_add (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { re = (a.re) + (b.re); im = (a.im) + (b.im) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec complex_mul (a: Complex) (b: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable a = a
    let mutable b = b
    try
        let real: float = ((a.re) * (b.re)) - ((a.im) * (b.im))
        let imag: float = ((a.re) * (b.im)) + ((a.im) * (b.re))
        __ret <- { re = real; im = imag }
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec complex_abs (a: Complex) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    try
        __ret <- sqrtApprox (((a.re) * (a.re)) + ((a.im) * (a.im)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec sin_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable i: int = 1
        while i < 10 do
            let k1: float = 2.0 * (float i)
            let k2: float = (2.0 * (float i)) + 1.0
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec cos_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            let k1: float = (2.0 * (float i)) - 1.0
            let k2: float = 2.0 * (float i)
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec exp_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: float = 1.0
        while i < 20.0 do
            term <- (term * x) / i
            sum <- sum + term
            i <- i + 1.0
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec complex_exp (z: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable z = z
    try
        let e: float = exp_taylor (z.re)
        __ret <- { re = e * (cos_taylor (z.im)); im = e * (sin_taylor (z.im)) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec eval_quadratic (c: Complex) (z: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable c = c
    let mutable z = z
    try
        __ret <- complex_add (complex_mul (z) (z)) (c)
        raise Return
        __ret
    with
        | Return -> __ret
let rec eval_exponential (c: Complex) (z: Complex) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable c = c
    let mutable z = z
    try
        __ret <- complex_add (complex_exp (z)) (c)
        raise Return
        __ret
    with
        | Return -> __ret
let rec iterate_function (eval_function: Complex -> Complex -> Complex) (c: Complex) (nb_iterations: int) (z0: Complex) (infinity: float) =
    let mutable __ret : Complex = Unchecked.defaultof<Complex>
    let mutable eval_function = eval_function
    let mutable c = c
    let mutable nb_iterations = nb_iterations
    let mutable z0 = z0
    let mutable infinity = infinity
    try
        let mutable z_n: Complex = z0
        let mutable i: int = 0
        while i < nb_iterations do
            z_n <- eval_function (c) (z_n)
            if (complex_abs (z_n)) > infinity then
                __ret <- z_n
                raise Return
            i <- i + 1
        __ret <- z_n
        raise Return
        __ret
    with
        | Return -> __ret
let rec prepare_grid (window_size: float) (nb_pixels: int) =
    let mutable __ret : Complex array array = Unchecked.defaultof<Complex array array>
    let mutable window_size = window_size
    let mutable nb_pixels = nb_pixels
    try
        let mutable grid: Complex array array = [||]
        let mutable i: int = 0
        while i < nb_pixels do
            let mutable row: Complex array = [||]
            let mutable j: int = 0
            while j < nb_pixels do
                let real: float = (-window_size) + (((2.0 * window_size) * (float i)) / (float (nb_pixels - 1)))
                let imag: float = (-window_size) + (((2.0 * window_size) * (float j)) / (float (nb_pixels - 1)))
                row <- Array.append row [|{ re = real; im = imag }|]
                j <- j + 1
            grid <- Array.append grid [|row|]
            i <- i + 1
        __ret <- grid
        raise Return
        __ret
    with
        | Return -> __ret
let rec julia_demo () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable grid: Complex array array = prepare_grid (1.0) (5)
        let c_poly: Complex = { re = -0.4; im = 0.6 }
        let c_exp: Complex = { re = -2.0; im = 0.0 }
        let mutable poly_result: int array array = [||]
        let mutable exp_result: int array array = [||]
        let mutable y: int = 0
        while y < (Seq.length (grid)) do
            let mutable row_poly: int array = [||]
            let mutable row_exp: int array = [||]
            let mutable x: int = 0
            while x < (Seq.length (_idx grid (y))) do
                let z0: Complex = _idx (_idx grid (y)) (x)
                let z_poly: Complex = iterate_function (unbox<Complex -> Complex -> Complex> eval_quadratic) (c_poly) (20) (z0) (4.0)
                let z_exp: Complex = iterate_function (unbox<Complex -> Complex -> Complex> eval_exponential) (c_exp) (10) (z0) (10000000000.0)
                row_poly <- Array.append row_poly [|(if (complex_abs (z_poly)) < 2.0 then 1 else 0)|]
                row_exp <- Array.append row_exp [|(if (complex_abs (z_exp)) < 10000.0 then 1 else 0)|]
                x <- x + 1
            poly_result <- Array.append poly_result [|row_poly|]
            exp_result <- Array.append exp_result [|row_exp|]
            y <- y + 1
        printfn "%s" (_repr (poly_result))
        printfn "%s" (_repr (exp_result))
        __ret
    with
        | Return -> __ret
julia_demo()
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
