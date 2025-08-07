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
let rec clamp_byte (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        if x < 0 then
            __ret <- 0
            raise Return
        if x > 255 then
            __ret <- 255
            raise Return
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec convert_to_negative (img: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let mutable out: int array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < w do
                row <- Array.append row [|255 - (_idx (_idx img (y)) (x))|]
                x <- x + 1
            out <- Array.append out [|row|]
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec change_contrast (img: int array array) (factor: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    let mutable factor = factor
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let mutable out: int array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < w do
                let p: int = _idx (_idx img (y)) (x)
                let mutable v: int = (((p - 128) * factor) / 100) + 128
                v <- clamp_byte (v)
                row <- Array.append row [|v|]
                x <- x + 1
            out <- Array.append out [|row|]
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec gen_gaussian_kernel (n: int) (sigma: float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable n = n
    let mutable sigma = sigma
    try
        if n = 3 then
            __ret <- [|[|1.0 / 16.0; 2.0 / 16.0; 1.0 / 16.0|]; [|2.0 / 16.0; 4.0 / 16.0; 2.0 / 16.0|]; [|1.0 / 16.0; 2.0 / 16.0; 1.0 / 16.0|]|]
            raise Return
        let mutable k: float array array = [||]
        let mutable i: int = 0
        while i < n do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < n do
                row <- Array.append row [|0.0|]
                j <- j + 1
            k <- Array.append k [|row|]
            i <- i + 1
        __ret <- k
        raise Return
        __ret
    with
        | Return -> __ret
let rec img_convolve (img: int array array) (kernel: float array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    let mutable kernel = kernel
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let mutable out: int array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < w do
                let mutable acc: float = 0.0
                let mutable ky: int = 0
                while ky < (Seq.length (kernel)) do
                    let mutable kx: int = 0
                    while kx < (Seq.length (_idx kernel (0))) do
                        let iy: int = (y + ky) - 1
                        let ix: int = (x + kx) - 1
                        let mutable pixel: int = 0
                        if (((iy >= 0) && (iy < h)) && (ix >= 0)) && (ix < w) then
                            pixel <- _idx (_idx img (iy)) (ix)
                        acc <- acc + ((_idx (_idx kernel (ky)) (kx)) * (1.0 * (float pixel)))
                        kx <- kx + 1
                    ky <- ky + 1
                row <- Array.append row [|unbox<int> (int (acc))|]
                x <- x + 1
            out <- Array.append out [|row|]
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec sort_ints (xs: int array) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable xs = xs
    try
        let mutable arr: int array = xs
        let mutable i: int = 0
        while i < (Seq.length (arr)) do
            let mutable j: int = 0
            while j < (((Seq.length (arr)) - 1) - i) do
                if (_idx arr (j)) > (_idx arr (j + 1)) then
                    let tmp: int = _idx arr (j)
                    arr.[j] <- _idx arr (j + 1)
                    arr.[j + 1] <- tmp
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec median_filter (img: int array array) (k: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    let mutable k = k
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let offset: int = k / 2
        let mutable out: int array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < w do
                let mutable vals: int array = [||]
                let mutable ky: int = 0
                while ky < k do
                    let mutable kx: int = 0
                    while kx < k do
                        let iy: int = (y + ky) - offset
                        let ix: int = (x + kx) - offset
                        let mutable pixel: int = 0
                        if (((iy >= 0) && (iy < h)) && (ix >= 0)) && (ix < w) then
                            pixel <- _idx (_idx img (iy)) (ix)
                        vals <- Array.append vals [|pixel|]
                        kx <- kx + 1
                    ky <- ky + 1
                let sorted: int array = sort_ints (vals)
                row <- Array.append row [|_idx sorted ((Seq.length (sorted)) / 2)|]
                x <- x + 1
            out <- Array.append out [|row|]
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec iabs (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x < 0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec sobel_filter (img: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    try
        let gx: int array array = [|[|1; 0; -1|]; [|2; 0; -2|]; [|1; 0; -1|]|]
        let gy: int array array = [|[|1; 2; 1|]; [|0; 0; 0|]; [|-1; -2; -1|]|]
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let mutable out: int array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < w do
                let mutable sx: int = 0
                let mutable sy: int = 0
                let mutable ky: int = 0
                while ky < 3 do
                    let mutable kx: int = 0
                    while kx < 3 do
                        let iy: int = (y + ky) - 1
                        let ix: int = (x + kx) - 1
                        let mutable pixel: int = 0
                        if (((iy >= 0) && (iy < h)) && (ix >= 0)) && (ix < w) then
                            pixel <- _idx (_idx img (iy)) (ix)
                        sx <- sx + ((_idx (_idx gx (ky)) (kx)) * pixel)
                        sy <- sy + ((_idx (_idx gy (ky)) (kx)) * pixel)
                        kx <- kx + 1
                    ky <- ky + 1
                row <- Array.append row [|(iabs (sx)) + (iabs (sy))|]
                x <- x + 1
            out <- Array.append out [|row|]
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_neighbors_pixel (img: int array array) (x: int) (y: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable img = img
    let mutable x = x
    let mutable y = y
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let mutable neighbors: int array = [||]
        let mutable dy: int = -1
        while dy <= 1 do
            let mutable dx: int = -1
            while dx <= 1 do
                if not ((dx = 0) && (dy = 0)) then
                    let ny: int = y + dy
                    let nx: int = x + dx
                    let mutable ``val``: int = 0
                    if (((ny >= 0) && (ny < h)) && (nx >= 0)) && (nx < w) then
                        ``val`` <- _idx (_idx img (ny)) (nx)
                    neighbors <- Array.append neighbors [|``val``|]
                dx <- dx + 1
            dy <- dy + 1
        __ret <- neighbors
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow2 (e: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable e = e
    try
        let mutable r: int = 1
        let mutable i: int = 0
        while i < e do
            r <- r * 2
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
let rec local_binary_value (img: int array array) (x: int) (y: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable img = img
    let mutable x = x
    let mutable y = y
    try
        let center: int = _idx (_idx img (y)) (x)
        let mutable neighbors: int array = get_neighbors_pixel (img) (x) (y)
        let mutable v: int = 0
        let mutable i: int = 0
        while i < (Seq.length (neighbors)) do
            if (_idx neighbors (i)) >= center then
                v <- v + (pow2 (i))
            i <- i + 1
        __ret <- v
        raise Return
        __ret
    with
        | Return -> __ret
let rec local_binary_pattern (img: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let mutable out: int array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < w do
                row <- Array.append row [|local_binary_value (img) (x) (y)|]
                x <- x + 1
            out <- Array.append out [|row|]
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let img: int array array = [|[|52; 55; 61|]; [|62; 59; 55|]; [|63; 65; 66|]|]
let negative: int array array = convert_to_negative (img)
let contrast: int array array = change_contrast (img) (110)
let kernel: float array array = gen_gaussian_kernel (3) (1.0)
let laplace: float array array = [|[|0.25; 0.5; 0.25|]; [|0.5; -3.0; 0.5|]; [|0.25; 0.5; 0.25|]|]
let convolved: int array array = img_convolve (img) (laplace)
let medianed: int array array = median_filter (img) (3)
let sobel: int array array = sobel_filter (img)
let lbp_img: int array array = local_binary_pattern (img)
printfn "%s" (_repr (negative))
printfn "%s" (_repr (contrast))
printfn "%s" (_repr (kernel))
printfn "%s" (_repr (convolved))
printfn "%s" (_repr (medianed))
printfn "%s" (_repr (sobel))
printfn "%s" (_repr (lbp_img))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
