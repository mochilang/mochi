// Generated 2025-07-26 04:38 +0700

exception Return

type Colour = {
    R: int
    G: int
    B: int
}
type Bitmap = {
    width: int
    height: int
    pixels: Colour array array
}
let rec newBitmap (w: int) (h: int) (c: Colour) =
    let mutable __ret : Bitmap = Unchecked.defaultof<Bitmap>
    let mutable w = w
    let mutable h = h
    let mutable c = c
    try
        let mutable rows: Colour array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: Colour array = [||]
            let mutable x: int = 0
            while x < w do
                row <- Array.append row [|c|]
                x <- x + 1
            rows <- Array.append rows [|row|]
            y <- y + 1
        __ret <- { width = w; height = h; pixels = rows }
        raise Return
        __ret
    with
        | Return -> __ret
and setPixel (b: Bitmap) (x: int) (y: int) (c: Colour) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable x = x
    let mutable y = y
    let mutable c = c
    try
        let mutable rows: Colour array array = b.pixels
        let mutable row: Colour array = rows.[y]
        row.[x] <- c
        rows.[y] <- row
        b <- { b with pixels = rows }
        __ret
    with
        | Return -> __ret
and fillRect (b: Bitmap) (x: int) (y: int) (w: int) (h: int) (c: Colour) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable x = x
    let mutable y = y
    let mutable w = w
    let mutable h = h
    let mutable c = c
    try
        let mutable yy: int = y
        while yy < (y + h) do
            let mutable xx: int = x
            while xx < (x + w) do
                setPixel b xx yy c
                xx <- xx + 1
            yy <- yy + 1
        __ret
    with
        | Return -> __ret
and pad (n: int) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable width = width
    try
        let mutable s: string = string n
        while (String.length s) < width do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and writePPMP3 (b: Bitmap) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        let mutable maxv: int = 0
        let mutable y: int = 0
        while y < (b.height) do
            let mutable x: int = 0
            while x < (b.width) do
                let p: Colour = ((b.pixels).[y]).[x]
                if (p.R) > maxv then
                    maxv <- p.R
                if (p.G) > maxv then
                    maxv <- p.G
                if (p.B) > maxv then
                    maxv <- p.B
                x <- x + 1
            y <- y + 1
        let mutable out: string = ((((("P3\n# generated from Bitmap.writeppmp3\n" + (string (b.width))) + " ") + (string (b.height))) + "\n") + (string maxv)) + "\n"
        let mutable numsize: int = String.length (string maxv)
        y <- (b.height) - 1
        while y >= 0 do
            let mutable line: string = ""
            let mutable x: int = 0
            while x < (b.width) do
                let p: Colour = ((b.pixels).[y]).[x]
                line <- (((((line + "   ") + (unbox<string> (pad (p.R) numsize))) + " ") + (unbox<string> (pad (p.G) numsize))) + " ") + (unbox<string> (pad (p.B) numsize))
                x <- x + 1
            out <- out + line
            if y > 0 then
                out <- out + "\n"
            else
                out <- out + "\n"
            y <- y - 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let black: Colour = { R = 0; G = 0; B = 0 }
        let white: Colour = { R = 255; G = 255; B = 255 }
        let mutable bm: Bitmap = newBitmap 4 4 black
        fillRect bm 1 0 1 2 white
        setPixel bm 3 3 { R = 127; G = 0; B = 63 }
        let ppm: string = writePPMP3 bm
        printfn "%s" ppm
        __ret
    with
        | Return -> __ret
main()
