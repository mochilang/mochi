// Generated 2025-07-26 10:43 +0700

exception Break
exception Continue

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

type Pixel = {
    R: int
    G: int
    B: int
}
type Bitmap = {
    w: int
    h: int
    max: int
    data: Pixel array array
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec newBitmap (w: int) (h: int) (max: int) =
    let mutable __ret : Bitmap = Unchecked.defaultof<Bitmap>
    let mutable w = w
    let mutable h = h
    let mutable max = max
    try
        let mutable rows: Pixel array array = [||]
        let mutable y: int = 0
        while y < h do
            let mutable row: Pixel array = [||]
            let mutable x: int = 0
            while x < w do
                row <- unbox<Pixel array> (Array.append row [|{ R = 0; G = 0; B = 0 }|])
                x <- x + 1
            rows <- unbox<Pixel array array> (Array.append rows [|row|])
            y <- y + 1
        __ret <- unbox<Bitmap> { w = w; h = h; max = max; data = rows }
        raise Return
        __ret
    with
        | Return -> __ret
let rec setPx (b: Bitmap) (x: int) (y: int) (p: Pixel) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable x = x
    let mutable y = y
    let mutable p = p
    try
        let mutable rows: Pixel array array = b.data
        let mutable row: Pixel array = rows.[y]
        row.[x] <- p
        rows.[y] <- row
        b <- { b with data = rows }
        __ret
    with
        | Return -> __ret
let rec getPx (b: Bitmap) (x: int) (y: int) =
    let mutable __ret : Pixel = Unchecked.defaultof<Pixel>
    let mutable b = b
    let mutable x = x
    let mutable y = y
    try
        __ret <- ((b.data).[y]).[x]
        raise Return
        __ret
    with
        | Return -> __ret
let rec splitLines (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable out: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if ch = "\n" then
                out <- unbox<string array> (Array.append out [|cur|])
                cur <- ""
            else
                cur <- cur + ch
            i <- i + 1
        out <- unbox<string array> (Array.append out [|cur|])
        __ret <- unbox<string array> out
        raise Return
        __ret
    with
        | Return -> __ret
let rec splitWS (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let mutable out: string array = [||]
        let mutable cur: string = ""
        let mutable i: int = 0
        while i < (String.length s) do
            let ch: string = _substring s i (i + 1)
            if (((ch = " ") || (ch = "\t")) || (ch = "\r")) || (ch = "\n") then
                if (String.length cur) > 0 then
                    out <- unbox<string array> (Array.append out [|cur|])
                    cur <- ""
            else
                cur <- cur + ch
            i <- i + 1
        if (String.length cur) > 0 then
            out <- unbox<string array> (Array.append out [|cur|])
        __ret <- unbox<string array> out
        raise Return
        __ret
    with
        | Return -> __ret
let rec parseIntStr (str: string) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable str = str
    try
        let mutable i: int = 0
        let mutable neg: bool = false
        if ((String.length str) > 0) && ((str.Substring(0, 1 - 0)) = "-") then
            neg <- true
            i <- 1
        let mutable n: int = 0
        let digits: Map<string, int> = Map.ofList [("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9)]
        while i < (String.length str) do
            n <- int ((n * 10) + (int (digits.[(str.Substring(i, (i + 1) - i))] |> unbox<int>)))
            i <- i + 1
        if neg then
            n <- -n
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
let rec tokenize (s: string) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable s = s
    try
        let lines: string array = splitLines s
        let mutable toks: string array = [||]
        let mutable i: int = 0
        try
            while i < (int (Array.length lines)) do
                let line: string = lines.[i]
                if ((String.length line) > 0) && ((_substring line 0 1) = "#") then
                    i <- i + 1
                    raise Continue
                let parts: string array = splitWS line
                let mutable j: int = 0
                while j < (int (Array.length parts)) do
                    toks <- unbox<string array> (Array.append toks [|parts.[j]|])
                    j <- j + 1
                i <- i + 1
        with
        | Break -> ()
        | Continue -> ()
        __ret <- unbox<string array> toks
        raise Return
        __ret
    with
        | Return -> __ret
let rec readP3 (text: string) =
    let mutable __ret : Bitmap = Unchecked.defaultof<Bitmap>
    let mutable text = text
    try
        let toks: string array = tokenize text
        if (int (Array.length toks)) < 4 then
            __ret <- unbox<Bitmap> (newBitmap 0 0 0)
            raise Return
        if (unbox<string> (toks.[0])) <> "P3" then
            __ret <- unbox<Bitmap> (newBitmap 0 0 0)
            raise Return
        let w: int = parseIntStr (unbox<string> (toks.[1]))
        let h: int = parseIntStr (unbox<string> (toks.[2]))
        let maxv: int = parseIntStr (unbox<string> (toks.[3]))
        let mutable idx: int = 4
        let mutable bm: Bitmap = newBitmap w h maxv
        let mutable y: int = h - 1
        while y >= 0 do
            let mutable x: int = 0
            while x < w do
                let r: int = parseIntStr (unbox<string> (toks.[idx]))
                let g: int = parseIntStr (unbox<string> (toks.[idx + 1]))
                let b: int = parseIntStr (unbox<string> (toks.[idx + 2]))
                setPx bm x y { R = r; G = g; B = b }
                idx <- idx + 3
                x <- x + 1
            y <- y - 1
        __ret <- bm
        raise Return
        __ret
    with
        | Return -> __ret
let rec toGrey (b: Bitmap) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    try
        let h: int = b.h
        let w: int = b.w
        let mutable m: int = 0
        let mutable y: int = 0
        while y < h do
            let mutable x: int = 0
            while x < w do
                let p: Pixel = getPx b x y
                let mutable l: int = ((((p.R) * 2126) + ((p.G) * 7152)) + ((p.B) * 722)) / 10000
                if l > (b.max) then
                    l <- b.max
                setPx b x y { R = l; G = l; B = l }
                if l > m then
                    m <- l
                x <- x + 1
            y <- y + 1
        b <- { b with max = m }
        __ret
    with
        | Return -> __ret
let rec pad (n: int) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    let mutable w = w
    try
        let mutable s: string = string n
        while (String.length s) < w do
            s <- " " + s
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec writeP3 (b: Bitmap) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable b = b
    try
        let h: int = b.h
        let w: int = b.w
        let mutable max: int = b.max
        let digits: int = String.length (string max)
        let mutable out: string = ((((("P3\n# generated from Bitmap.writeppmp3\n" + (string w)) + " ") + (string h)) + "\n") + (string max)) + "\n"
        let mutable y: int = h - 1
        while y >= 0 do
            let mutable line: string = ""
            let mutable x: int = 0
            while x < w do
                let p: Pixel = getPx b x y
                line <- (((((line + "   ") + (unbox<string> (pad (p.R) digits))) + " ") + (unbox<string> (pad (p.G) digits))) + " ") + (unbox<string> (pad (p.B) digits))
                x <- x + 1
            out <- (out + line) + "\n"
            y <- y - 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
let mutable ppmtxt: string = (((((("P3\n" + "# feep.ppm\n") + "4 4\n") + "15\n") + " 0  0  0    0  0  0    0  0  0   15  0 15\n") + " 0  0  0    0 15  7    0  0  0    0  0  0\n") + " 0  0  0    0  0  0    0 15  7    0  0  0\n") + "15  0 15    0  0  0    0  0  0    0  0  0\n"
printfn "%s" "Original Colour PPM file"
printfn "%s" ppmtxt
let mutable bm: Bitmap = readP3 ppmtxt
printfn "%s" "Grey PPM:"
toGrey bm
let out: string = writeP3 bm
printfn "%s" out
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
