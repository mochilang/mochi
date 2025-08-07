// Generated 2025-08-07 16:27 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type RGB = {
    r: int
    g: int
    b: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec round_int (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- int (x + 0.5)
        raise Return
        __ret
    with
        | Return -> __ret
let rec hsv_to_rgb (h: float) (s: float) (v: float) =
    let mutable __ret : RGB = Unchecked.defaultof<RGB>
    let mutable h = h
    let mutable s = s
    let mutable v = v
    try
        let i: int = int (h * 6.0)
        let f: float = (h * 6.0) - (float i)
        let p: float = v * (1.0 - s)
        let q: float = v * (1.0 - (f * s))
        let t: float = v * (1.0 - ((1.0 - f) * s))
        let ``mod``: int = ((i % 6 + 6) % 6)
        let mutable r: float = 0.0
        let mutable g: float = 0.0
        let mutable b: float = 0.0
        if ``mod`` = 0 then
            r <- v
            g <- t
            b <- p
        else
            if ``mod`` = 1 then
                r <- q
                g <- v
                b <- p
            else
                if ``mod`` = 2 then
                    r <- p
                    g <- v
                    b <- t
                else
                    if ``mod`` = 3 then
                        r <- p
                        g <- q
                        b <- v
                    else
                        if ``mod`` = 4 then
                            r <- t
                            g <- p
                            b <- v
                        else
                            r <- v
                            g <- p
                            b <- q
        __ret <- { r = round_int (r * 255.0); g = round_int (g * 255.0); b = round_int (b * 255.0) }
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_distance (x: float) (y: float) (max_step: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    let mutable max_step = max_step
    try
        let mutable a: float = x
        let mutable b: float = y
        let mutable step: int = -1
        try
            while step < (max_step - 1) do
                try
                    step <- step + 1
                    let a_new: float = ((a * a) - (b * b)) + x
                    b <- ((2.0 * a) * b) + y
                    a <- a_new
                    if ((a * a) + (b * b)) > 4.0 then
                        raise Break
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret <- (float step) / (float (max_step - 1))
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_black_and_white_rgb (distance: float) =
    let mutable __ret : RGB = Unchecked.defaultof<RGB>
    let mutable distance = distance
    try
        if distance = 1.0 then
            __ret <- { r = 0; g = 0; b = 0 }
            raise Return
        else
            __ret <- { r = 255; g = 255; b = 255 }
            raise Return
        __ret
    with
        | Return -> __ret
let rec get_color_coded_rgb (distance: float) =
    let mutable __ret : RGB = Unchecked.defaultof<RGB>
    let mutable distance = distance
    try
        if distance = 1.0 then
            __ret <- { r = 0; g = 0; b = 0 }
            raise Return
        else
            __ret <- hsv_to_rgb (distance) (1.0) (1.0)
            raise Return
        __ret
    with
        | Return -> __ret
let rec get_image (image_width: int) (image_height: int) (figure_center_x: float) (figure_center_y: float) (figure_width: float) (max_step: int) (use_distance_color_coding: bool) =
    let mutable __ret : RGB array array = Unchecked.defaultof<RGB array array>
    let mutable image_width = image_width
    let mutable image_height = image_height
    let mutable figure_center_x = figure_center_x
    let mutable figure_center_y = figure_center_y
    let mutable figure_width = figure_width
    let mutable max_step = max_step
    let mutable use_distance_color_coding = use_distance_color_coding
    try
        let mutable img: RGB array array = [||]
        let figure_height: float = (figure_width / (float image_width)) * (float image_height)
        let mutable image_y: int = 0
        while image_y < image_height do
            let mutable row: RGB array = [||]
            let mutable image_x: int = 0
            while image_x < image_width do
                let fx: float = figure_center_x + ((((float image_x) / (float image_width)) - 0.5) * figure_width)
                let fy: float = figure_center_y + ((((float image_y) / (float image_height)) - 0.5) * figure_height)
                let distance: float = get_distance (fx) (fy) (max_step)
                let mutable rgb: RGB = Unchecked.defaultof<RGB>
                if use_distance_color_coding then
                    rgb <- get_color_coded_rgb (distance)
                else
                    rgb <- get_black_and_white_rgb (distance)
                row <- Array.append row [|rgb|]
                image_x <- image_x + 1
            img <- Array.append img [|row|]
            image_y <- image_y + 1
        __ret <- img
        raise Return
        __ret
    with
        | Return -> __ret
let rec rgb_to_string (c: RGB) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        __ret <- ((((("(" + (_str (c.r))) + ", ") + (_str (c.g))) + ", ") + (_str (c.b))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let img1: RGB array array = get_image (10) (10) (-0.6) (0.0) (3.2) (50) (true)
printfn "%s" (rgb_to_string (_idx (_idx img1 (0)) (0)))
let img2: RGB array array = get_image (10) (10) (-0.6) (0.0) (3.2) (50) (false)
printfn "%s" (rgb_to_string (_idx (_idx img2 (0)) (0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
