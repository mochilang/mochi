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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
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
open System.Collections.Generic

let PI: float = 3.141592653589793
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
let rec atanApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x > 1.0 then
            __ret <- (PI / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        if x < (-1.0) then
            __ret <- ((-PI) / 2.0) - (x / ((x * x) + 0.28))
            raise Return
        __ret <- x / (1.0 + ((0.28 * x) * x))
        raise Return
        __ret
    with
        | Return -> __ret
let rec atan2Approx (y: float) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y = y
    let mutable x = x
    try
        if x > 0.0 then
            let mutable r: float = atanApprox (y / x)
            __ret <- r
            raise Return
        if x < 0.0 then
            if y >= 0.0 then
                __ret <- (atanApprox (y / x)) + PI
                raise Return
            __ret <- (atanApprox (y / x)) - PI
            raise Return
        if y > 0.0 then
            __ret <- PI / 2.0
            raise Return
        if y < 0.0 then
            __ret <- (-PI) / 2.0
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec deg (rad: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable rad = rad
    try
        __ret <- (rad * 180.0) / PI
        raise Return
        __ret
    with
        | Return -> __ret
let GAUSSIAN_KERNEL: float array array = [|[|0.0625; 0.125; 0.0625|]; [|0.125; 0.25; 0.125|]; [|0.0625; 0.125; 0.0625|]|]
let SOBEL_GX: float array array = [|[|-1.0; 0.0; 1.0|]; [|-2.0; 0.0; 2.0|]; [|-1.0; 0.0; 1.0|]|]
let SOBEL_GY: float array array = [|[|1.0; 2.0; 1.0|]; [|0.0; 0.0; 0.0|]; [|-1.0; -2.0; -1.0|]|]
let rec zero_matrix (h: int) (w: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable h = h
    let mutable w = w
    try
        let mutable out: float array array = [||]
        let mutable i: int = 0
        while i < h do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < w do
                row <- Array.append row [|0.0|]
                j <- j + 1
            out <- Array.append out [|row|]
            i <- i + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec convolve (img: float array array) (kernel: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable img = img
    let mutable kernel = kernel
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let k: int = Seq.length (kernel)
        let pad: int = k / 2
        let mutable out: float array array = zero_matrix (h) (w)
        let mutable y: int = pad
        while y < (h - pad) do
            let mutable x: int = pad
            while x < (w - pad) do
                let mutable sum: float = 0.0
                let mutable ky: int = 0
                while ky < k do
                    let mutable kx: int = 0
                    while kx < k do
                        let pixel: float = _idx (_idx img ((y - pad) + ky)) ((x - pad) + kx)
                        let weight: float = _idx (_idx kernel (ky)) (kx)
                        sum <- sum + (pixel * weight)
                        kx <- kx + 1
                    ky <- ky + 1
                out.[y].[x] <- sum
                x <- x + 1
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec gaussian_blur (img: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable img = img
    try
        __ret <- convolve (img) (GAUSSIAN_KERNEL)
        raise Return
        __ret
    with
        | Return -> __ret
let rec sobel_filter (img: float array array) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float array array> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float array array>>
    let mutable img = img
    try
        let gx: float array array = convolve (img) (SOBEL_GX)
        let gy: float array array = convolve (img) (SOBEL_GY)
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let mutable grad: float array array = zero_matrix (h) (w)
        let mutable dir: float array array = zero_matrix (h) (w)
        let mutable i: int = 0
        while i < h do
            let mutable j: int = 0
            while j < w do
                let gxx: float = _idx (_idx gx (i)) (j)
                let gyy: float = _idx (_idx gy (i)) (j)
                grad.[i].[j] <- sqrtApprox ((gxx * gxx) + (gyy * gyy))
                dir.[i].[j] <- (deg (atan2Approx (gyy) (gxx))) + 180.0
                j <- j + 1
            i <- i + 1
        __ret <- _dictCreate [("grad", grad); ("dir", dir)]
        raise Return
        __ret
    with
        | Return -> __ret
let rec suppress_non_maximum (h: int) (w: int) (direction: float array array) (grad: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable h = h
    let mutable w = w
    let mutable direction = direction
    let mutable grad = grad
    try
        let mutable dest: float array array = zero_matrix (h) (w)
        let mutable r: int = 1
        while r < (h - 1) do
            let mutable c: int = 1
            while c < (w - 1) do
                let angle: float = _idx (_idx direction (r)) (c)
                let mutable q: float = 0.0
                let mutable p: float = 0.0
                if (((angle >= 0.0) && (angle < 22.5)) || ((angle >= 157.5) && (angle <= 180.0))) || (angle >= 337.5) then
                    q <- _idx (_idx grad (r)) (c + 1)
                    p <- _idx (_idx grad (r)) (c - 1)
                else
                    if ((angle >= 22.5) && (angle < 67.5)) || ((angle >= 202.5) && (angle < 247.5)) then
                        q <- _idx (_idx grad (r + 1)) (c - 1)
                        p <- _idx (_idx grad (r - 1)) (c + 1)
                    else
                        if ((angle >= 67.5) && (angle < 112.5)) || ((angle >= 247.5) && (angle < 292.5)) then
                            q <- _idx (_idx grad (r + 1)) (c)
                            p <- _idx (_idx grad (r - 1)) (c)
                        else
                            q <- _idx (_idx grad (r - 1)) (c - 1)
                            p <- _idx (_idx grad (r + 1)) (c + 1)
                if ((_idx (_idx grad (r)) (c)) >= q) && ((_idx (_idx grad (r)) (c)) >= p) then
                    dest.[r].[c] <- _idx (_idx grad (r)) (c)
                c <- c + 1
            r <- r + 1
        __ret <- dest
        raise Return
        __ret
    with
        | Return -> __ret
let rec double_threshold (h: int) (w: int) (img: float array array) (low: float) (high: float) (weak: float) (strong: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable w = w
    let mutable img = img
    let mutable low = low
    let mutable high = high
    let mutable weak = weak
    let mutable strong = strong
    try
        let mutable r: int = 0
        while r < h do
            let mutable c: int = 0
            while c < w do
                let v: float = _idx (_idx img (r)) (c)
                if v >= high then
                    img.[r].[c] <- strong
                else
                    if v < low then
                        img.[r].[c] <- 0.0
                    else
                        img.[r].[c] <- weak
                c <- c + 1
            r <- r + 1
        __ret
    with
        | Return -> __ret
let rec track_edge (h: int) (w: int) (img: float array array) (weak: float) (strong: float) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable h = h
    let mutable w = w
    let mutable img = img
    let mutable weak = weak
    let mutable strong = strong
    try
        let mutable r: int = 1
        while r < (h - 1) do
            let mutable c: int = 1
            while c < (w - 1) do
                if (_idx (_idx img (r)) (c)) = weak then
                    if ((((((((_idx (_idx img (r + 1)) (c)) = strong) || ((_idx (_idx img (r - 1)) (c)) = strong)) || ((_idx (_idx img (r)) (c + 1)) = strong)) || ((_idx (_idx img (r)) (c - 1)) = strong)) || ((_idx (_idx img (r - 1)) (c - 1)) = strong)) || ((_idx (_idx img (r - 1)) (c + 1)) = strong)) || ((_idx (_idx img (r + 1)) (c - 1)) = strong)) || ((_idx (_idx img (r + 1)) (c + 1)) = strong) then
                        img.[r].[c] <- strong
                    else
                        img.[r].[c] <- 0.0
                c <- c + 1
            r <- r + 1
        __ret
    with
        | Return -> __ret
let rec canny (image: float array array) (low: float) (high: float) (weak: float) (strong: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable image = image
    let mutable low = low
    let mutable high = high
    let mutable weak = weak
    let mutable strong = strong
    try
        let blurred: float array array = gaussian_blur (image)
        let sob: System.Collections.Generic.IDictionary<string, float array array> = sobel_filter (blurred)
        let grad: float array array = sob.[(string ("grad"))]
        let direction: float array array = sob.[(string ("dir"))]
        let h: int = Seq.length (image)
        let w: int = Seq.length (_idx image (0))
        let suppressed: float array array = suppress_non_maximum (h) (w) (direction) (grad)
        double_threshold (h) (w) (suppressed) (low) (high) (weak) (strong)
        track_edge (h) (w) (suppressed) (weak) (strong)
        __ret <- suppressed
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_image (img: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable img = img
    try
        let mutable r: int = 0
        while r < (Seq.length (img)) do
            let mutable c: int = 0
            let mutable line: string = ""
            while c < (Seq.length (_idx img (r))) do
                line <- (line + (_str (int (_idx (_idx img (r)) (c))))) + " "
                c <- c + 1
            printfn "%s" (line)
            r <- r + 1
        __ret
    with
        | Return -> __ret
let image: float array array = [|[|0.0; 0.0; 0.0; 0.0; 0.0|]; [|0.0; 255.0; 255.0; 255.0; 0.0|]; [|0.0; 255.0; 255.0; 255.0; 0.0|]; [|0.0; 255.0; 255.0; 255.0; 0.0|]; [|0.0; 0.0; 0.0; 0.0; 0.0|]|]
let edges: float array array = canny (image) (20.0) (40.0) (128.0) (255.0)
print_image (edges)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
