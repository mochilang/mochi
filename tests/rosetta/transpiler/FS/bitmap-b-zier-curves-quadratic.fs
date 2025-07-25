// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

type Pixel = {
    r: int
    g: int
    b: int
}
let b2Seg: int = 20
let rec pixelFromRgb (rgb: int) =
    let mutable __ret : Pixel = Unchecked.defaultof<Pixel>
    let mutable rgb = rgb
    try
        let r: int = int ((((rgb / 65536) % 256 + 256) % 256))
        let g: int = int ((((rgb / 256) % 256 + 256) % 256))
        let b: int = int (((rgb % 256 + 256) % 256))
        __ret <- { r = r; g = g; b = b }
        raise Return
        __ret
    with
        | Return -> __ret
and newBitmap (cols: int) (rows: int) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable cols = cols
    let mutable rows = rows
    try
        let mutable d: Pixel array array = [||]
        let mutable y: int = 0
        while y < rows do
            let mutable row: Pixel array = [||]
            let mutable x: int = 0
            while x < cols do
                row <- Array.append row [|{ r = 0; g = 0; b = 0 }|]
                x <- x + 1
            d <- Array.append d [|row|]
            y <- y + 1
        __ret <- Map.ofList [("cols", box cols); ("rows", box rows); ("data", box d)]
        raise Return
        __ret
    with
        | Return -> __ret
and setPx (b: Map<string, obj>) (x: int) (y: int) (p: Pixel) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable x = x
    let mutable y = y
    let mutable p = p
    try
        let cols: int = int (b.["cols"])
        let rows: int = int (b.["rows"])
        if (((x >= 0) && (x < cols)) && (y >= 0)) && (y < rows) then
            b <- Map.add "data" (Map.add y (Map.add x p ((b.["data"]).[y])) (b.["data"])) b
        __ret
    with
        | Return -> __ret
and fill (b: Map<string, obj>) (p: Pixel) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable p = p
    try
        let cols: int = int (b.["cols"])
        let rows: int = int (b.["rows"])
        let mutable y: int = 0
        while y < rows do
            let mutable x: int = 0
            while x < cols do
                b <- Map.add "data" (Map.add y (Map.add x p ((b.["data"]).[y])) (b.["data"])) b
                x <- x + 1
            y <- y + 1
        __ret
    with
        | Return -> __ret
and fillRgb (b: Map<string, obj>) (rgb: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable rgb = rgb
    try
        fill b (unbox<Pixel> (pixelFromRgb rgb))
        __ret
    with
        | Return -> __ret
and line (b: Map<string, obj>) (x0: int) (y0: int) (x1: int) (y1: int) (p: Pixel) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable x0 = x0
    let mutable y0 = y0
    let mutable x1 = x1
    let mutable y1 = y1
    let mutable p = p
    try
        let mutable dx: int = x1 - x0
        if dx < 0 then
            dx <- -dx
        let mutable dy: int = y1 - y0
        if dy < 0 then
            dy <- -dy
        let mutable sx: int = -1
        if x0 < x1 then
            sx <- 1
        let mutable sy: int = -1
        if y0 < y1 then
            sy <- 1
        let mutable err: int = dx - dy
        try
            while true do
                setPx b x0 y0 p
                if (x0 = x1) && (y0 = y1) then
                    raise Break
                let e2: int = 2 * err
                if e2 > (0 - dy) then
                    err <- err - dy
                    x0 <- x0 + sx
                if e2 < dx then
                    err <- err + dx
                    y0 <- y0 + sy
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and bezier2 (b: Map<string, obj>) (x1: int) (y1: int) (x2: int) (y2: int) (x3: int) (y3: int) (p: Pixel) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable x1 = x1
    let mutable y1 = y1
    let mutable x2 = x2
    let mutable y2 = y2
    let mutable x3 = x3
    let mutable y3 = y3
    let mutable p = p
    try
        let mutable px: int array = [||]
        let mutable py: int array = [||]
        let mutable i: int = 0
        while i <= b2Seg do
            px <- Array.append px [|0|]
            py <- Array.append py [|0|]
            i <- i + 1
        let fx1: float = float x1
        let fy1: float = float y1
        let fx2: float = float x2
        let fy2: float = float y2
        let fx3: float = float x3
        let fy3: float = float y3
        i <- 0
        while i <= b2Seg do
            let c: float = (float i) / (float b2Seg)
            let mutable a: float = 1.0 - c
            let mutable a2: float = a * a
            let mutable b2: float = (2.0 * c) * a
            let mutable c2: float = c * c
            px.[i] <- int (((a2 * fx1) + (b2 * fx2)) + (c2 * fx3))
            py.[i] <- int (((a2 * fy1) + (b2 * fy2)) + (c2 * fy3))
            i <- i + 1
        let mutable x0: int = px.[0]
        let mutable y0: int = py.[0]
        i <- 1
        while i <= b2Seg do
            let x: int = px.[i]
            let y: int = py.[i]
            line b x0 y0 x y p
            x0 <- x
            y0 <- y
            i <- i + 1
        __ret
    with
        | Return -> __ret
let mutable b: Map<string, obj> = newBitmap 400 300
fillRgb b 14614575
bezier2 b 20 150 500 (-100) 300 280 (unbox<Pixel> (pixelFromRgb 4165615))
