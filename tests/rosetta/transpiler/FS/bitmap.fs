// Generated 2025-07-26 04:38 +0700

exception Return

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
let rec pixelFromRgb (c: int) =
    let mutable __ret : Pixel = Unchecked.defaultof<Pixel>
    let mutable c = c
    try
        let r: int = (((int (c / 65536)) % 256 + 256) % 256)
        let g: int = (((int (c / 256)) % 256 + 256) % 256)
        let b: int = ((c % 256 + 256) % 256)
        __ret <- { R = r; G = g; B = b }
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
                r <- Array.append r [|{ R = 0; G = 0; B = 0 }|]
                col <- col + 1
            data <- Array.append data [|r|]
            row <- row + 1
        __ret <- { cols = x; rows = y; px = data }
        raise Return
        __ret
    with
        | Return -> __ret
and Extent (b: Bitmap) =
    let mutable __ret : Map<string, int> = Unchecked.defaultof<Map<string, int>>
    let mutable b = b
    try
        __ret <- Map.ofList [("cols", b.cols); ("rows", b.rows)]
        raise Return
        __ret
    with
        | Return -> __ret
and Fill (b: Bitmap) (p: Pixel) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable p = p
    try
        let mutable y: int = 0
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
and FillRgb (b: Bitmap) (c: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable c = c
    try
        Fill b (unbox<Pixel> (pixelFromRgb c))
        __ret
    with
        | Return -> __ret
and SetPx (b: Bitmap) (x: int) (y: int) (p: Pixel) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable b = b
    let mutable x = x
    let mutable y = y
    let mutable p = p
    try
        if (((x < 0) || (x >= (b.cols))) || (y < 0)) || (y >= (b.rows)) then
            __ret <- false
            raise Return
        let mutable px: Pixel array array = b.px
        let mutable row: Pixel array = px.[y]
        row.[x] <- p
        px.[y] <- row
        b <- { b with px = px }
        __ret <- true
        raise Return
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
        __ret <- SetPx b x y (unbox<Pixel> (pixelFromRgb c))
        raise Return
        __ret
    with
        | Return -> __ret
and GetPx (b: Bitmap) (x: int) (y: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable b = b
    let mutable x = x
    let mutable y = y
    try
        if (((x < 0) || (x >= (b.cols))) || (y < 0)) || (y >= (b.rows)) then
            __ret <- Map.ofList [("ok", box false)]
            raise Return
        let row: Pixel array = (b.px).[y]
        __ret <- Map.ofList [("ok", box true); ("pixel", box (row.[x]))]
        raise Return
        __ret
    with
        | Return -> __ret
and GetPxRgb (b: Bitmap) (x: int) (y: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable b = b
    let mutable x = x
    let mutable y = y
    try
        let r: Map<string, obj> = GetPx b x y
        if not (r.ok) then
            __ret <- Map.ofList [("ok", box false)]
            raise Return
        __ret <- Map.ofList [("ok", box true); ("rgb", box (rgbFromPixel (unbox<Pixel> (r.pixel))))]
        raise Return
        __ret
    with
        | Return -> __ret
and ppmSize (b: Bitmap) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable b = b
    try
        let header: string = ((("P6\n# Creator: Rosetta Code http://rosettacode.org/\n" + (string (b.cols))) + " ") + (string (b.rows))) + "\n255\n"
        __ret <- (String.length header) + ((3 * (b.cols)) * (b.rows))
        raise Return
        __ret
    with
        | Return -> __ret
and pixelStr (p: Pixel) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        __ret <- ((((("{" + (string (p.R))) + " ") + (string (p.G))) + " ") + (string (p.B))) + "}"
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable bm: Bitmap = NewBitmap 300 240
        FillRgb bm 16711680
        SetPxRgb bm 10 20 255
        SetPxRgb bm 20 30 0
        SetPxRgb bm 30 40 1056816
        let c1: Map<string, obj> = GetPx bm 0 0
        let c2: Map<string, obj> = GetPx bm 10 20
        let c3: Map<string, obj> = GetPx bm 30 40
        printfn "%s" ((("Image size: " + (string (bm.cols))) + " × ") + (string (bm.rows)))
        printfn "%s" ((string (ppmSize bm)) + " bytes when encoded as PPM.")
        if unbox<bool> (c1.ok) then
            printfn "%s" ("Pixel at (0,0) is " + (unbox<string> (pixelStr (unbox<Pixel> (c1.pixel)))))
        if unbox<bool> (c2.ok) then
            printfn "%s" ("Pixel at (10,20) is " + (unbox<string> (pixelStr (unbox<Pixel> (c2.pixel)))))
        if unbox<bool> (c3.ok) then
            let p = c3.pixel
            let mutable r16 = (int (p.R)) * 257
            let mutable g16 = (int (p.G)) * 257
            let mutable b16 = (int (p.B)) * 257
            printfn "%s" ((((("Pixel at (30,40) has R=" + (string r16)) + ", G=") + (string g16)) + ", B=") + (string b16))
        __ret
    with
        | Return -> __ret
main()
