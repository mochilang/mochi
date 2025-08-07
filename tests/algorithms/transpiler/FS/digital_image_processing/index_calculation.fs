// Generated 2025-08-07 15:46 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec ndvi (red: float) (nir: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable red = red
    let mutable nir = nir
    try
        __ret <- (nir - red) / (nir + red)
        raise Return
        __ret
    with
        | Return -> __ret
and bndvi (blue: float) (nir: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable blue = blue
    let mutable nir = nir
    try
        __ret <- (nir - blue) / (nir + blue)
        raise Return
        __ret
    with
        | Return -> __ret
and gndvi (green: float) (nir: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable green = green
    let mutable nir = nir
    try
        __ret <- (nir - green) / (nir + green)
        raise Return
        __ret
    with
        | Return -> __ret
and ndre (redEdge: float) (nir: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable redEdge = redEdge
    let mutable nir = nir
    try
        __ret <- (nir - redEdge) / (nir + redEdge)
        raise Return
        __ret
    with
        | Return -> __ret
and ccci (red: float) (redEdge: float) (nir: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable red = red
    let mutable redEdge = redEdge
    let mutable nir = nir
    try
        __ret <- (ndre (redEdge) (nir)) / (ndvi (red) (nir))
        raise Return
        __ret
    with
        | Return -> __ret
and cvi (red: float) (green: float) (nir: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable red = red
    let mutable green = green
    let mutable nir = nir
    try
        __ret <- (nir * red) / (green * green)
        raise Return
        __ret
    with
        | Return -> __ret
and gli (red: float) (green: float) (blue: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable red = red
    let mutable green = green
    let mutable blue = blue
    try
        __ret <- (((2.0 * green) - red) - blue) / (((2.0 * green) + red) + blue)
        raise Return
        __ret
    with
        | Return -> __ret
and dvi (red: float) (nir: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable red = red
    let mutable nir = nir
    try
        __ret <- nir / red
        raise Return
        __ret
    with
        | Return -> __ret
and calc (index: string) (red: float) (green: float) (blue: float) (redEdge: float) (nir: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable index = index
    let mutable red = red
    let mutable green = green
    let mutable blue = blue
    let mutable redEdge = redEdge
    let mutable nir = nir
    try
        if index = "NDVI" then
            __ret <- ndvi (red) (nir)
            raise Return
        if index = "BNDVI" then
            __ret <- bndvi (blue) (nir)
            raise Return
        if index = "GNDVI" then
            __ret <- gndvi (green) (nir)
            raise Return
        if index = "NDRE" then
            __ret <- ndre (redEdge) (nir)
            raise Return
        if index = "CCCI" then
            __ret <- ccci (red) (redEdge) (nir)
            raise Return
        if index = "CVI" then
            __ret <- cvi (red) (green) (nir)
            raise Return
        if index = "GLI" then
            __ret <- gli (red) (green) (blue)
            raise Return
        if index = "DVI" then
            __ret <- dvi (red) (nir)
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let red: float = 50.0
        let green: float = 30.0
        let blue: float = 10.0
        let redEdge: float = 40.0
        let nir: float = 100.0
        printfn "%s" ("NDVI=" + (_str (ndvi (red) (nir))))
        printfn "%s" ("CCCI=" + (_str (ccci (red) (redEdge) (nir))))
        printfn "%s" ("CVI=" + (_str (cvi (red) (green) (nir))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
