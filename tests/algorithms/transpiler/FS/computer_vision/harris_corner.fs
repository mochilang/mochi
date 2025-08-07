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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec zeros (h: int) (w: int) =
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
let rec gradient (img: int array array) =
    let mutable __ret : float array array array = Unchecked.defaultof<float array array array>
    let mutable img = img
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let mutable dx: float array array = zeros (h) (w)
        let mutable dy: float array array = zeros (h) (w)
        let mutable y: int = 1
        while y < (h - 1) do
            let mutable x: int = 1
            while x < (w - 1) do
                dx.[y].[x] <- (System.Convert.ToDouble (_idx (_idx img (y)) (x + 1))) - (System.Convert.ToDouble (_idx (_idx img (y)) (x - 1)))
                dy.[y].[x] <- (System.Convert.ToDouble (_idx (_idx img (y + 1)) (x))) - (System.Convert.ToDouble (_idx (_idx img (y - 1)) (x)))
                x <- x + 1
            y <- y + 1
        __ret <- [|dx; dy|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec harris (img: int array array) (k: float) (window: int) (thresh: float) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    let mutable k = k
    let mutable window = window
    let mutable thresh = thresh
    try
        let h: int = Seq.length (img)
        let w: int = Seq.length (_idx img (0))
        let grads: float array array array = gradient (img)
        let dx: float array array = _idx grads (0)
        let dy: float array array = _idx grads (1)
        let mutable ixx: float array array = zeros (h) (w)
        let mutable iyy: float array array = zeros (h) (w)
        let mutable ixy: float array array = zeros (h) (w)
        let mutable y: int = 0
        while y < h do
            let mutable x: int = 0
            while x < w do
                let gx: float = _idx (_idx dx (y)) (x)
                let gy: float = _idx (_idx dy (y)) (x)
                ixx.[y].[x] <- gx * gx
                iyy.[y].[x] <- gy * gy
                ixy.[y].[x] <- gx * gy
                x <- x + 1
            y <- y + 1
        let offset: int = window / 2
        let mutable corners: int array array = [||]
        y <- offset
        while y < (h - offset) do
            let mutable x: int = offset
            while x < (w - offset) do
                let mutable wxx: float = 0.0
                let mutable wyy: float = 0.0
                let mutable wxy: float = 0.0
                let mutable yy: int = y - offset
                while yy <= (y + offset) do
                    let mutable xx: int = x - offset
                    while xx <= (x + offset) do
                        wxx <- wxx + (_idx (_idx ixx (yy)) (xx))
                        wyy <- wyy + (_idx (_idx iyy (yy)) (xx))
                        wxy <- wxy + (_idx (_idx ixy (yy)) (xx))
                        xx <- xx + 1
                    yy <- yy + 1
                let det: float = (wxx * wyy) - (wxy * wxy)
                let trace: float = wxx + wyy
                let r: float = det - (k * (trace * trace))
                if r > thresh then
                    corners <- Array.append corners [|unbox<int array> [|x; y|]|]
                x <- x + 1
            y <- y + 1
        __ret <- corners
        raise Return
        __ret
    with
        | Return -> __ret
let img: int array array = [|[|1; 1; 1; 1; 1|]; [|1; 255; 255; 255; 1|]; [|1; 255; 0; 255; 1|]; [|1; 255; 255; 255; 1|]; [|1; 1; 1; 1; 1|]|]
let mutable corners: int array array = harris (img) (0.04) (3) (10000000000.0)
printfn "%s" (_repr (corners))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
