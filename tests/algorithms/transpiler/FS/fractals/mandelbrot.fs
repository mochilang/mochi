// Generated 2025-08-13 16:13 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type RGB = {
    mutable _r: int
    mutable _g: int
    mutable _b: int
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
and hsv_to_rgb (h: float) (s: float) (v: float) =
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
        let mutable _r: float = 0.0
        let mutable _g: float = 0.0
        let mutable _b: float = 0.0
        if ``mod`` = 0 then
            _r <- v
            _g <- t
            _b <- p
        else
            if ``mod`` = 1 then
                _r <- q
                _g <- v
                _b <- p
            else
                if ``mod`` = 2 then
                    _r <- p
                    _g <- v
                    _b <- t
                else
                    if ``mod`` = 3 then
                        _r <- p
                        _g <- q
                        _b <- v
                    else
                        if ``mod`` = 4 then
                            _r <- t
                            _g <- p
                            _b <- v
                        else
                            _r <- v
                            _g <- p
                            _b <- q
        __ret <- { _r = round_int (_r * 255.0); _g = round_int (_g * 255.0); _b = round_int (_b * 255.0) }
        raise Return
        __ret
    with
        | Return -> __ret
and get_distance (x: float) (y: float) (max_step: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    let mutable max_step = max_step
    try
        let mutable a: float = x
        let mutable _b: float = y
        let mutable step: int = -1
        try
            while step < (max_step - 1) do
                try
                    step <- step + 1
                    let a_new: float = ((a * a) - (_b * _b)) + x
                    _b <- ((2.0 * a) * _b) + y
                    a <- a_new
                    if ((a * a) + (_b * _b)) > 4.0 then
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
and get_black_and_white_rgb (distance: float) =
    let mutable __ret : RGB = Unchecked.defaultof<RGB>
    let mutable distance = distance
    try
        if distance = 1.0 then
            __ret <- { _r = 0; _g = 0; _b = 0 }
            raise Return
        else
            __ret <- { _r = 255; _g = 255; _b = 255 }
            raise Return
        __ret
    with
        | Return -> __ret
and get_color_coded_rgb (distance: float) =
    let mutable __ret : RGB = Unchecked.defaultof<RGB>
    let mutable distance = distance
    try
        if distance = 1.0 then
            __ret <- { _r = 0; _g = 0; _b = 0 }
            raise Return
        else
            __ret <- hsv_to_rgb (distance) (1.0) (1.0)
            raise Return
        __ret
    with
        | Return -> __ret
and get_image (image_width: int) (image_height: int) (figure_center_x: float) (figure_center_y: float) (figure_width: float) (max_step: int) (use_distance_color_coding: bool) =
    let mutable __ret : RGB array array = Unchecked.defaultof<RGB array array>
    let mutable image_width = image_width
    let mutable image_height = image_height
    let mutable figure_center_x = figure_center_x
    let mutable figure_center_y = figure_center_y
    let mutable figure_width = figure_width
    let mutable max_step = max_step
    let mutable use_distance_color_coding = use_distance_color_coding
    try
        let mutable img: RGB array array = Array.empty<RGB array>
        let figure_height: float = (figure_width / (float image_width)) * (float image_height)
        let mutable image_y: int = 0
        while image_y < image_height do
            let mutable row: RGB array = Array.empty<RGB>
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
and rgb_to_string (c: RGB) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable c = c
    try
        __ret <- ((((("(" + (_str (c._r))) + ", ") + (_str (c._g))) + ", ") + (_str (c._b))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let img1: RGB array array = get_image (10) (10) (-0.6) (0.0) (3.2) (50) (true)
ignore (printfn "%s" (rgb_to_string (_idx (_idx img1 (int 0)) (int 0))))
let img2: RGB array array = get_image (10) (10) (-0.6) (0.0) (3.2) (50) (false)
ignore (printfn "%s" (rgb_to_string (_idx (_idx img2 (int 0)) (int 0))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
