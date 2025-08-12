// Generated 2025-08-12 07:47 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _floordiv (a:int) (b:int) : int =
    let q = a / b
    let r = a % b
    if r <> 0 && ((a < 0) <> (b < 0)) then q - 1 else q
let rec pow2_int (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < n do
            result <- int ((int64 result) * (int64 2))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and pow2_float (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable result: float = 1.0
        if n >= 0 then
            let mutable i: int = 0
            while i < n do
                result <- result * 2.0
                i <- i + 1
        else
            let mutable i: int = 0
            let m: int = 0 - n
            while i < m do
                result <- result / 2.0
                i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and lshift (num: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    let mutable k = k
    try
        let mutable result: int = num
        let mutable i: int = 0
        while i < k do
            result <- int ((int64 result) * (int64 2))
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and rshift (num: int) (k: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    let mutable k = k
    try
        let mutable result: int = num
        let mutable i: int = 0
        while i < k do
            result <- _floordiv (result - (((result % 2 + 2) % 2))) 2
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and log2_floor (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable n: float = x
        let mutable e: int = 0
        while n >= 2.0 do
            n <- n / 2.0
            e <- e + 1
        while n < 1.0 do
            n <- n * 2.0
            e <- e - 1
        __ret <- e
        raise Return
        __ret
    with
        | Return -> __ret
and float_to_bits (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable num: float = x
        let mutable sign: int = 0
        if num < 0.0 then
            sign <- 1
            num <- -num
        let exp: int = log2_floor (num)
        let pow: float = pow2_float (exp)
        let normalized: float = num / pow
        let frac: float = normalized - 1.0
        let mantissa: int = int (frac * (pow2_float (23)))
        let exp_bits: int = exp + 127
        __ret <- ((lshift (sign) (31)) + (lshift (exp_bits) (23))) + mantissa
        raise Return
        __ret
    with
        | Return -> __ret
and bits_to_float (bits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable bits = bits
    try
        let sign_bit: int = (((rshift (bits) (31)) % 2 + 2) % 2)
        let mutable sign: float = 1.0
        if sign_bit = 1 then
            sign <- -1.0
        let exp_bits: int = (((rshift (bits) (23)) % 256 + 256) % 256)
        let exp: int = exp_bits - 127
        let mantissa_bits: int = ((bits % (pow2_int (23)) + (pow2_int (23))) % (pow2_int (23)))
        let mantissa: float = 1.0 + ((float mantissa_bits) / (pow2_float (23)))
        __ret <- (sign * mantissa) * (pow2_float (exp))
        raise Return
        __ret
    with
        | Return -> __ret
and absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
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
and is_close (a: float) (b: float) (rel_tol: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable rel_tol = rel_tol
    try
        __ret <- (absf (a - b)) <= (rel_tol * (absf (b)))
        raise Return
        __ret
    with
        | Return -> __ret
and fast_inverse_sqrt (number: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable number = number
    try
        if number <= 0.0 then
            failwith ("Input must be a positive number.")
        let mutable i: int = float_to_bits (number)
        let magic: int = 1597463007
        let y_bits: int = magic - (rshift (i) (1))
        let mutable y: float = bits_to_float (y_bits)
        y <- y * (1.5 - (((0.5 * number) * y) * y))
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
and test_fast_inverse_sqrt () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (absf ((fast_inverse_sqrt (10.0)) - 0.3156857923527257)) > 0.0001 then
            failwith ("fast_inverse_sqrt(10) failed")
        if (absf ((fast_inverse_sqrt (4.0)) - 0.49915357479239103)) > 0.0001 then
            failwith ("fast_inverse_sqrt(4) failed")
        if (absf ((fast_inverse_sqrt (4.1)) - 0.4932849504615651)) > 0.0001 then
            failwith ("fast_inverse_sqrt(4.1) failed")
        let mutable i: int = 50
        while i < 60 do
            let mutable y: float = fast_inverse_sqrt (float i)
            let actual: float = 1.0 / (sqrtApprox (float i))
            if not (is_close (y) (actual) (0.00132)) then
                failwith ("relative error too high")
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_fast_inverse_sqrt()
        let mutable i: int = 5
        while i <= 100 do
            let diff: float = (1.0 / (sqrtApprox (float i))) - (fast_inverse_sqrt (float i))
            printfn "%s" (((_str (i)) + ": ") + (_str (diff)))
            i <- i + 5
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
