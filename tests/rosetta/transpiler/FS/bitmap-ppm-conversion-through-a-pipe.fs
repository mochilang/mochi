// Generated 2025-07-26 10:43 +0700

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
type Pixel = {
    R: int
    G: int
    B: int
}
type Bitmap = {
    cols: int
    rows: int
    px: Pixel array array
}
open System

let rec pixelFromRgb (c: int) =
    let mutable __ret : Pixel = Unchecked.defaultof<Pixel>
    let mutable c = c
    try
        let r: int = (((int (c / 65536)) % 256 + 256) % 256)
        let g: int = (((int (c / 256)) % 256 + 256) % 256)
        let b: int = ((c % 256 + 256) % 256)
        __ret <- unbox<Pixel> { R = r; G = g; B = b }
        raise Return
        __ret
    with
        | Return -> __ret
and rgbFromPixel (p: Pixel) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable p = p
    try
        __ret <- (((p.R) * 65536) + ((p.G) * 256)) + (p.B)
        raise Return
        __ret
    with
        | Return -> __ret
and NewBitmap (x: int) (y: int) =
    let mutable __ret : Bitmap = Unchecked.defaultof<Bitmap>
    let mutable x = x
    let mutable y = y
    try
        let mutable data: Pixel array array = [||]
        let mutable row: int = 0
        while row < y do
            let mutable r: Pixel array = [||]
            let mutable col: int = 0
            while col < x do
                r <- unbox<Pixel array> (Array.append r [|{ R = 0; G = 0; B = 0 }|])
                col <- col + 1
            data <- unbox<Pixel array array> (Array.append data [|r|])
            row <- row + 1
        __ret <- unbox<Bitmap> { cols = x; rows = y; px = data }
        raise Return
        __ret
    with
        | Return -> __ret
and FillRgb (b: Bitmap) (c: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable c = c
    try
        let mutable y: int = 0
        let p: Pixel = pixelFromRgb c
        while y < (b.rows) do
            let mutable x: int = 0
            while x < (b.cols) do
                let mutable px: Pixel array array = b.px
                let mutable row: Pixel array = px.[y]
                row.[x] <- p
                px.[y] <- row
                b <- { b with px = px }
                x <- x + 1
            y <- y + 1
        __ret
    with
        | Return -> __ret
and SetPxRgb (b: Bitmap) (x: int) (y: int) (c: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable b = b
    let mutable x = x
    let mutable y = y
    let mutable c = c
    try
        if (((x < 0) || (x >= (b.cols))) || (y < 0)) || (y >= (b.rows)) then
            __ret <- false
            raise Return
        let mutable px: Pixel array array = b.px
        let mutable row: Pixel array = px.[y]
        row.[x] <- pixelFromRgb c
        px.[y] <- row
        b <- { b with px = px }
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and nextRand (seed: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable seed = seed
    try
        __ret <- int ((((int64 ((seed * 1664525) + 1013904223)) % 2147483648L + 2147483648L) % 2147483648L))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable bm: Bitmap = NewBitmap 400 300
        FillRgb bm 12615744
        let mutable seed: int = _now()
        let mutable i: int = 0
        while i < 2000 do
            seed <- int (nextRand seed)
            let x: int = ((seed % 400 + 400) % 400)
            seed <- int (nextRand seed)
            let y: int = ((seed % 300 + 300) % 300)
            SetPxRgb bm x y 8405024
            i <- i + 1
        let mutable x: int = 0
        while x < 400 do
            let mutable y: int = 240
            while y < 245 do
                SetPxRgb bm x y 8405024
                y <- y + 1
            y <- 260
            while y < 265 do
                SetPxRgb bm x y 8405024
                y <- y + 1
            x <- x + 1
        let mutable y: int = 0
        while y < 300 do
            let mutable x: int = 80
            while x < 85 do
                SetPxRgb bm x y 8405024
                x <- x + 1
            x <- 95
            while x < 100 do
                SetPxRgb bm x y 8405024
                x <- x + 1
            y <- y + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
