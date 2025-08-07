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
let rec round_int (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x >= 0.0 then (int (x + 0.5)) else (int (x - 0.5))
        raise Return
        __ret
    with
        | Return -> __ret
and zeros (rows: int) (cols: int) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable rows = rows
    let mutable cols = cols
    try
        let mutable res: float array array = [||]
        let mutable i: int = 0
        while i < rows do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < cols do
                row <- Array.append row [|0.0|]
                j <- j + 1
            res <- Array.append res [|row|]
            i <- i + 1
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
and warp (image: float array array) (h_flow: float array array) (v_flow: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable image = image
    let mutable h_flow = h_flow
    let mutable v_flow = v_flow
    try
        let h: int = Seq.length (image)
        let w: int = Seq.length (_idx image (0))
        let mutable out: float array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: float array = [||]
            let mutable x: int = 0
            while x < w do
                let sx: int = x - (round_int (_idx (_idx h_flow (y)) (x)))
                let sy: int = y - (round_int (_idx (_idx v_flow (y)) (x)))
                if (((sx >= 0) && (sx < w)) && (sy >= 0)) && (sy < h) then
                    row <- Array.append row [|_idx (_idx image (sy)) (sx)|]
                else
                    row <- Array.append row [|0.0|]
                x <- x + 1
            out <- Array.append out [|row|]
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and convolve (img: float array array) (ker: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable img = img
    let mutable ker = ker
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let kh: int = Seq.length (ker)
        let kw: int = Seq.length (_idx ker (0))
        let py: int = kh / 2
        let px: int = kw / 2
        let mutable out: float array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: float array = [||]
            let mutable x: int = 0
            while x < w do
                let mutable s: float = 0.0
                let mutable ky: int = 0
                while ky < kh do
                    let mutable kx: int = 0
                    while kx < kw do
                        let iy: int = (y + ky) - py
                        let ix: int = (x + kx) - px
                        if (((iy >= 0) && (iy < h)) && (ix >= 0)) && (ix < w) then
                            s <- s + ((_idx (_idx img (iy)) (ix)) * (_idx (_idx ker (ky)) (kx)))
                        kx <- kx + 1
                    ky <- ky + 1
                row <- Array.append row [|s|]
                x <- x + 1
            out <- Array.append out [|row|]
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and horn_schunck (image0: float array array) (image1: float array array) (num_iter: int) (alpha: float) =
    let mutable __ret : float array array array = Unchecked.defaultof<float array array array>
    let mutable image0 = image0
    let mutable image1 = image1
    let mutable num_iter = num_iter
    let mutable alpha = alpha
    try
        let h: int = Seq.length (image0)
        let w: int = Seq.length (_idx image0 (0))
        let mutable u: float array array = zeros (h) (w)
        let mutable v: float array array = zeros (h) (w)
        let kernel_x: float array array = [|[|-0.25; 0.25|]; [|-0.25; 0.25|]|]
        let kernel_y: float array array = [|[|-0.25; -0.25|]; [|0.25; 0.25|]|]
        let kernel_t: float array array = [|[|0.25; 0.25|]; [|0.25; 0.25|]|]
        let laplacian: float array array = [|[|0.0833333333333; 0.166666666667; 0.0833333333333|]; [|0.166666666667; 0.0; 0.166666666667|]; [|0.0833333333333; 0.166666666667; 0.0833333333333|]|]
        let mutable it: int = 0
        while it < num_iter do
            let warped: float array array = warp (image0) (u) (v)
            let dx1: float array array = convolve (warped) (kernel_x)
            let dx2: float array array = convolve (image1) (kernel_x)
            let dy1: float array array = convolve (warped) (kernel_y)
            let dy2: float array array = convolve (image1) (kernel_y)
            let dt1: float array array = convolve (warped) (kernel_t)
            let dt2: float array array = convolve (image1) (kernel_t)
            let avg_u: float array array = convolve (u) (laplacian)
            let avg_v: float array array = convolve (v) (laplacian)
            let mutable y: int = 0
            while y < h do
                let mutable x: int = 0
                while x < w do
                    let dx: float = (_idx (_idx dx1 (y)) (x)) + (_idx (_idx dx2 (y)) (x))
                    let dy: float = (_idx (_idx dy1 (y)) (x)) + (_idx (_idx dy2 (y)) (x))
                    let dt: float = (_idx (_idx dt1 (y)) (x)) - (_idx (_idx dt2 (y)) (x))
                    let au: float = _idx (_idx avg_u (y)) (x)
                    let av: float = _idx (_idx avg_v (y)) (x)
                    let numer: float = ((dx * au) + (dy * av)) + dt
                    let denom: float = ((alpha * alpha) + (dx * dx)) + (dy * dy)
                    let upd: float = numer / denom
                    u.[y].[x] <- au - (dx * upd)
                    v.[y].[x] <- av - (dy * upd)
                    x <- x + 1
                y <- y + 1
            it <- it + 1
        __ret <- [|u; v|]
        raise Return
        __ret
    with
        | Return -> __ret
and print_matrix (mat: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        let mutable y: int = 0
        while y < (Seq.length (mat)) do
            let mutable row: float array = _idx mat (y)
            let mutable x: int = 0
            let mutable line: string = ""
            while x < (Seq.length (row)) do
                line <- line + (_str (round_int (_idx row (x))))
                if (x + 1) < (Seq.length (row)) then
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
        let image0: float array array = [|[|0.0; 0.0; 2.0|]; [|0.0; 0.0; 2.0|]|]
        let image1: float array array = [|[|0.0; 2.0; 0.0|]; [|0.0; 2.0; 0.0|]|]
        let flows: float array array array = horn_schunck (image0) (image1) (20) (0.1)
        let u: float array array = _idx flows (0)
        let v: float array array = _idx flows (1)
        print_matrix (u)
        printfn "%s" ("---")
        print_matrix (v)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
