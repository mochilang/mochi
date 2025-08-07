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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec fmod (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- a - (float (b * (float (int (a / b)))))
        raise Return
        __ret
    with
        | Return -> __ret
let rec roundf (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x >= 0.0 then (int (x + 0.5)) else (int (x - 0.5))
        raise Return
        __ret
    with
        | Return -> __ret
let rec maxf (a: float) (b: float) (c: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let mutable m: float = a
        if b > m then
            m <- b
        if c > m then
            m <- c
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec minf (a: float) (b: float) (c: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let mutable m: float = a
        if b < m then
            m <- b
        if c < m then
            m <- c
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
let rec hsv_to_rgb (hue: float) (saturation: float) (value: float) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable hue = hue
    let mutable saturation = saturation
    let mutable value = value
    try
        if (hue < 0.0) || (hue > 360.0) then
            printfn "%s" ("hue should be between 0 and 360")
            __ret <- Array.empty<int>
            raise Return
        if (saturation < 0.0) || (saturation > 1.0) then
            printfn "%s" ("saturation should be between 0 and 1")
            __ret <- Array.empty<int>
            raise Return
        if (value < 0.0) || (value > 1.0) then
            printfn "%s" ("value should be between 0 and 1")
            __ret <- Array.empty<int>
            raise Return
        let chroma: float = value * saturation
        let hue_section: float = hue / 60.0
        let second_largest_component: float = chroma * (1.0 - (absf ((fmod (hue_section) (2.0)) - 1.0)))
        let match_value: float = value - chroma
        let mutable red: int = 0
        let mutable green: int = 0
        let mutable blue: int = 0
        if (hue_section >= 0.0) && (hue_section <= 1.0) then
            red <- roundf (255.0 * (chroma + match_value))
            green <- roundf (255.0 * (second_largest_component + match_value))
            blue <- roundf (255.0 * match_value)
        else
            if (hue_section > 1.0) && (hue_section <= 2.0) then
                red <- roundf (255.0 * (second_largest_component + match_value))
                green <- roundf (255.0 * (chroma + match_value))
                blue <- roundf (255.0 * match_value)
            else
                if (hue_section > 2.0) && (hue_section <= 3.0) then
                    red <- roundf (255.0 * match_value)
                    green <- roundf (255.0 * (chroma + match_value))
                    blue <- roundf (255.0 * (second_largest_component + match_value))
                else
                    if (hue_section > 3.0) && (hue_section <= 4.0) then
                        red <- roundf (255.0 * match_value)
                        green <- roundf (255.0 * (second_largest_component + match_value))
                        blue <- roundf (255.0 * (chroma + match_value))
                    else
                        if (hue_section > 4.0) && (hue_section <= 5.0) then
                            red <- roundf (255.0 * (second_largest_component + match_value))
                            green <- roundf (255.0 * match_value)
                            blue <- roundf (255.0 * (chroma + match_value))
                        else
                            red <- roundf (255.0 * (chroma + match_value))
                            green <- roundf (255.0 * match_value)
                            blue <- roundf (255.0 * (second_largest_component + match_value))
        __ret <- unbox<int array> [|red; green; blue|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec rgb_to_hsv (red: int) (green: int) (blue: int) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable red = red
    let mutable green = green
    let mutable blue = blue
    try
        if (red < 0) || (red > 255) then
            printfn "%s" ("red should be between 0 and 255")
            __ret <- Array.empty<float>
            raise Return
        if (green < 0) || (green > 255) then
            printfn "%s" ("green should be between 0 and 255")
            __ret <- Array.empty<float>
            raise Return
        if (blue < 0) || (blue > 255) then
            printfn "%s" ("blue should be between 0 and 255")
            __ret <- Array.empty<float>
            raise Return
        let float_red: float = (float red) / 255.0
        let float_green: float = (float green) / 255.0
        let float_blue: float = (float blue) / 255.0
        let value: float = maxf (float_red) (float_green) (float_blue)
        let min_val: float = minf (float_red) (float_green) (float_blue)
        let chroma: float = value - min_val
        let saturation: float = if value = 0.0 then 0.0 else (chroma / value)
        let mutable hue: float = 0
        if chroma = 0.0 then
            hue <- 0.0
        else
            if value = float_red then
                hue <- 60.0 * (0.0 + ((float_green - float_blue) / chroma))
            else
                if value = float_green then
                    hue <- 60.0 * (2.0 + ((float_blue - float_red) / chroma))
                else
                    hue <- 60.0 * (4.0 + ((float_red - float_green) / chroma))
        hue <- fmod (hue + 360.0) (360.0)
        __ret <- unbox<float array> [|hue; saturation; value|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec approximately_equal_hsv (hsv1: float array) (hsv2: float array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable hsv1 = hsv1
    let mutable hsv2 = hsv2
    try
        let check_hue: bool = (absf ((_idx hsv1 (0)) - (_idx hsv2 (0)))) < 0.2
        let check_saturation: bool = (absf ((_idx hsv1 (1)) - (_idx hsv2 (1)))) < 0.002
        let check_value: bool = (absf ((_idx hsv1 (2)) - (_idx hsv2 (2)))) < 0.002
        __ret <- (check_hue && check_saturation) && check_value
        raise Return
        __ret
    with
        | Return -> __ret
let rgb: int array = hsv_to_rgb (180.0) (0.5) (0.5)
printfn "%s" (_str (rgb))
let hsv: float array = rgb_to_hsv (64) (128) (128)
printfn "%s" (_str (hsv))
printfn "%s" (_str (approximately_equal_hsv (hsv) (unbox<float array> [|180.0; 0.5; 0.5|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
