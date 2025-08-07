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
let PI: float = 3.141592653589793
let rec absf (x: float) =
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
and atanApprox (x: float) =
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
and atan2Approx (y: float) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable y = y
    let mutable x = x
    try
        if x = 0.0 then
            if y > 0.0 then
                __ret <- PI / 2.0
                raise Return
            if y < 0.0 then
                __ret <- (-PI) / 2.0
                raise Return
            __ret <- 0.0
            raise Return
        let a: float = atanApprox (y / x)
        if x > 0.0 then
            __ret <- a
            raise Return
        if y >= 0.0 then
            __ret <- a + PI
            raise Return
        __ret <- a - PI
        raise Return
        __ret
    with
        | Return -> __ret
and zeros (h: int) (w: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable h = h
    let mutable w = w
    try
        let mutable m: float array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: float array = [||]
            let mutable x: int = 0
            while x < w do
                row <- Array.append row [|0.0|]
                x <- x + 1
            m <- Array.append m [|row|]
            y <- y + 1
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and pad_edge (img: float array array) (pad: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable img = img
    let mutable pad = pad
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let mutable out: float array array = zeros (h + (pad * 2)) (w + (pad * 2))
        let mutable y: int = 0
        while y < (h + (pad * 2)) do
            let mutable x: int = 0
            while x < (w + (pad * 2)) do
                let mutable sy: int = y - pad
                if sy < 0 then
                    sy <- 0
                if sy >= h then
                    sy <- h - 1
                let mutable sx: int = x - pad
                if sx < 0 then
                    sx <- 0
                if sx >= w then
                    sx <- w - 1
                out.[y].[x] <- _idx (_idx img (sy)) (sx)
                x <- x + 1
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and img_convolve (img: float array array) (kernel: int array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable img = img
    let mutable kernel = kernel
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let k: int = Seq.length (kernel)
        let pad: int = k / 2
        let padded: float array array = pad_edge (img) (pad)
        let mutable out: float array array = zeros (h) (w)
        let mutable y: int = 0
        while y < h do
            let mutable x: int = 0
            while x < w do
                let mutable sum: float = 0.0
                let mutable i: int = 0
                while i < k do
                    let mutable j: int = 0
                    while j < k do
                        sum <- sum + ((_idx (_idx padded (y + i)) (x + j)) * (float (_idx (_idx kernel (i)) (j))))
                        j <- j + 1
                    i <- i + 1
                out.[y].[x] <- sum
                x <- x + 1
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and abs_matrix (mat: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    try
        let h: int = Seq.length (mat)
        let w: int = Seq.length (_idx mat (0))
        let mutable out: float array array = zeros (h) (w)
        let mutable y: int = 0
        while y < h do
            let mutable x: int = 0
            while x < w do
                let v: float = _idx (_idx mat (y)) (x)
                if v < 0.0 then
                    out.[y].[x] <- -v
                else
                    out.[y].[x] <- v
                x <- x + 1
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and max_matrix (mat: float array array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable mat = mat
    try
        let mutable max_val: float = _idx (_idx mat (0)) (0)
        let mutable y: int = 0
        while y < (Seq.length (mat)) do
            let mutable x: int = 0
            while x < (Seq.length (_idx mat (0))) do
                if (_idx (_idx mat (y)) (x)) > max_val then
                    max_val <- _idx (_idx mat (y)) (x)
                x <- x + 1
            y <- y + 1
        __ret <- max_val
        raise Return
        __ret
    with
        | Return -> __ret
and scale_matrix (mat: float array array) (factor: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable mat = mat
    let mutable factor = factor
    try
        let h: int = Seq.length (mat)
        let w: int = Seq.length (_idx mat (0))
        let mutable out: float array array = zeros (h) (w)
        let mutable y: int = 0
        while y < h do
            let mutable x: int = 0
            while x < w do
                out.[y].[x] <- (_idx (_idx mat (y)) (x)) * factor
                x <- x + 1
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and sobel_filter (image: int array array) =
    let mutable __ret : float array array array = Unchecked.defaultof<float array array array>
    let mutable image = image
    try
        let h: int = Seq.length (image)
        let w: int = Seq.length (_idx image (0))
        let mutable img: float array array = [||]
        let mutable y0: int = 0
        while y0 < h do
            let mutable row: float array = [||]
            let mutable x0: int = 0
            while x0 < w do
                row <- Array.append row [|float (_idx (_idx image (y0)) (x0))|]
                x0 <- x0 + 1
            img <- Array.append img [|row|]
            y0 <- y0 + 1
        let kernel_x: int array array = [|[|-1; 0; 1|]; [|-2; 0; 2|]; [|-1; 0; 1|]|]
        let kernel_y: int array array = [|[|1; 2; 1|]; [|0; 0; 0|]; [|-1; -2; -1|]|]
        let mutable dst_x: float array array = abs_matrix (img_convolve (img) (kernel_x))
        let mutable dst_y: float array array = abs_matrix (img_convolve (img) (kernel_y))
        let max_x: float = max_matrix (dst_x)
        let max_y: float = max_matrix (dst_y)
        dst_x <- scale_matrix (dst_x) (255.0 / max_x)
        dst_y <- scale_matrix (dst_y) (255.0 / max_y)
        let mutable mag: float array array = zeros (h) (w)
        let mutable theta: float array array = zeros (h) (w)
        let mutable y: int = 0
        while y < h do
            let mutable x: int = 0
            while x < w do
                let gx: float = _idx (_idx dst_x (y)) (x)
                let gy: float = _idx (_idx dst_y (y)) (x)
                mag.[y].[x] <- sqrtApprox ((gx * gx) + (gy * gy))
                theta.[y].[x] <- atan2Approx (gy) (gx)
                x <- x + 1
            y <- y + 1
        let max_m: float = max_matrix (mag)
        mag <- scale_matrix (mag) (255.0 / max_m)
        __ret <- [|mag; theta|]
        raise Return
        __ret
    with
        | Return -> __ret
and print_matrix_int (mat: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        let mutable y: int = 0
        while y < (Seq.length (mat)) do
            let mutable line: string = ""
            let mutable x: int = 0
            while x < (Seq.length (_idx mat (y))) do
                line <- line + (_str (int (_idx (_idx mat (y)) (x))))
                if x < ((Seq.length (_idx mat (y))) - 1) then
                    line <- line + " "
                x <- x + 1
            printfn "%s" (line)
            y <- y + 1
        __ret
    with
        | Return -> __ret
and print_matrix_float (mat: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        let mutable y: int = 0
        while y < (Seq.length (mat)) do
            let mutable line: string = ""
            let mutable x: int = 0
            while x < (Seq.length (_idx mat (y))) do
                line <- line + (_str (_idx (_idx mat (y)) (x)))
                if x < ((Seq.length (_idx mat (y))) - 1) then
                    line <- line + " "
                x <- x + 1
            printfn "%s" (line)
            y <- y + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable img: int array array = [|[|10; 10; 10; 10; 10|]; [|10; 50; 50; 50; 10|]; [|10; 50; 80; 50; 10|]; [|10; 50; 50; 50; 10|]; [|10; 10; 10; 10; 10|]|]
        let res: float array array array = sobel_filter (img)
        let mutable mag: float array array = _idx res (0)
        let theta: float array array = _idx res (1)
        print_matrix_int (mag)
        print_matrix_float (theta)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
