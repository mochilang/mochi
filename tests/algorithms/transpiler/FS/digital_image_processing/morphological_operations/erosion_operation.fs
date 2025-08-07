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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec rgb_to_gray (rgb: int array array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable rgb = rgb
    try
        let mutable gray: float array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (rgb)) do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx rgb (i))) do
                let r: float = float (_idx (_idx (_idx rgb (i)) (j)) (0))
                let g: float = float (_idx (_idx (_idx rgb (i)) (j)) (1))
                let b: float = float (_idx (_idx (_idx rgb (i)) (j)) (2))
                let value: float = ((0.2989 * r) + (0.587 * g)) + (0.114 * b)
                row <- Array.append row [|value|]
                j <- j + 1
            gray <- Array.append gray [|row|]
            i <- i + 1
        __ret <- gray
        raise Return
        __ret
    with
        | Return -> __ret
let rec gray_to_binary (gray: float array array) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable gray = gray
    try
        let mutable binary: bool array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (gray)) do
            let mutable row: bool array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx gray (i))) do
                row <- Array.append row [|((_idx (_idx gray (i)) (j)) > 127.0) && ((_idx (_idx gray (i)) (j)) <= 255.0)|]
                j <- j + 1
            binary <- Array.append binary [|row|]
            i <- i + 1
        __ret <- binary
        raise Return
        __ret
    with
        | Return -> __ret
let rec erosion (image: bool array array) (kernel: int array array) =
    let mutable __ret : bool array array = Unchecked.defaultof<bool array array>
    let mutable image = image
    let mutable kernel = kernel
    try
        let h: int = Seq.length (image)
        let w: int = Seq.length (_idx image (0))
        let k_h: int = Seq.length (kernel)
        let k_w: int = Seq.length (_idx kernel (0))
        let pad_y: int = k_h / 2
        let pad_x: int = k_w / 2
        let mutable padded: bool array array = [||]
        let mutable y: int = 0
        while y < (h + (2 * pad_y)) do
            let mutable row: bool array = [||]
            let mutable x: int = 0
            while x < (w + (2 * pad_x)) do
                row <- Array.append row [|false|]
                x <- x + 1
            padded <- Array.append padded [|row|]
            y <- y + 1
        y <- 0
        while y < h do
            let mutable x: int = 0
            while x < w do
                padded.[pad_y + y].[pad_x + x] <- _idx (_idx image (y)) (x)
                x <- x + 1
            y <- y + 1
        let mutable output: bool array array = [||]
        y <- 0
        while y < h do
            let mutable row_out: bool array = [||]
            let mutable x: int = 0
            while x < w do
                let mutable sum: int = 0
                let mutable ky: int = 0
                while ky < k_h do
                    let mutable kx: int = 0
                    while kx < k_w do
                        if ((_idx (_idx kernel (ky)) (kx)) = 1) && (_idx (_idx padded (y + ky)) (x + kx)) then
                            sum <- sum + 1
                        kx <- kx + 1
                    ky <- ky + 1
                row_out <- Array.append row_out [|sum = 5|]
                x <- x + 1
            output <- Array.append output [|row_out|]
            y <- y + 1
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
let rgb_img: int array array array = [|[|[|127; 255; 0|]|]|]
printfn "%s" (_str (rgb_to_gray (rgb_img)))
let gray_img: float array array = [|[|127.0; 255.0; 0.0|]|]
printfn "%s" (_str (gray_to_binary (gray_img)))
let img1: bool array array = [|[|true; true; false|]|]
let kernel1: int array array = [|[|0; 1; 0|]|]
printfn "%s" (_str (erosion (img1) (kernel1)))
let img2: bool array array = [|[|true; false; false|]|]
let kernel2: int array array = [|[|1; 1; 0|]|]
printfn "%s" (_str (erosion (img2) (kernel2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
