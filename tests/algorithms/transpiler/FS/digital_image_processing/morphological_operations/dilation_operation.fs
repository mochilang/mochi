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
let rec rgb_to_gray (rgb: int array array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable rgb = rgb
    try
        let mutable result: float array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (rgb)) do
            let mutable row: float array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx rgb (i))) do
                let r: int = _idx (_idx (_idx rgb (i)) (j)) (0)
                let g: int = _idx (_idx (_idx rgb (i)) (j)) (1)
                let b: int = _idx (_idx (_idx rgb (i)) (j)) (2)
                let gray: float = ((0.2989 * (1.0 * (float r))) + (0.587 * (1.0 * (float g)))) + (0.114 * (1.0 * (float b)))
                row <- Array.append row [|gray|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and gray_to_binary (gray: float array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable gray = gray
    try
        let mutable result: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (gray)) do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (_idx gray (i))) do
                let v: float = _idx (_idx gray (i)) (j)
                if (v > 127.0) && (v <= 255.0) then
                    row <- Array.append row [|1|]
                else
                    row <- Array.append row [|0|]
                j <- j + 1
            result <- Array.append result [|row|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and dilation (image: int array array) (kernel: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable image = image
    let mutable kernel = kernel
    try
        let img_h: int = Seq.length (image)
        let img_w: int = Seq.length (_idx image (0))
        let k_h: int = Seq.length (kernel)
        let k_w: int = Seq.length (_idx kernel (0))
        let pad_h: int = k_h / 2
        let pad_w: int = k_w / 2
        let p_h: int = img_h + (2 * pad_h)
        let p_w: int = img_w + (2 * pad_w)
        let mutable padded: int array array = [||]
        let mutable i: int = 0
        while i < p_h do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < p_w do
                row <- Array.append row [|0|]
                j <- j + 1
            padded <- Array.append padded [|row|]
            i <- i + 1
        i <- 0
        while i < img_h do
            let mutable j: int = 0
            while j < img_w do
                padded.[pad_h + i].[pad_w + j] <- _idx (_idx image (i)) (j)
                j <- j + 1
            i <- i + 1
        let mutable output: int array array = [||]
        i <- 0
        while i < img_h do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < img_w do
                let mutable sum: int = 0
                let mutable ky: int = 0
                while ky < k_h do
                    let mutable kx: int = 0
                    while kx < k_w do
                        if (_idx (_idx kernel (ky)) (kx)) = 1 then
                            sum <- sum + (_idx (_idx padded (i + ky)) (j + kx))
                        kx <- kx + 1
                    ky <- ky + 1
                if sum > 0 then
                    row <- Array.append row [|1|]
                else
                    row <- Array.append row [|0|]
                j <- j + 1
            output <- Array.append output [|row|]
            i <- i + 1
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
and print_float_matrix (mat: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (i))) do
                line <- line + (_str (_idx (_idx mat (i)) (j)))
                if j < ((Seq.length (_idx mat (i))) - 1) then
                    line <- line + " "
                j <- j + 1
            printfn "%s" (line)
            i <- i + 1
        __ret
    with
        | Return -> __ret
and print_int_matrix (mat: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable mat = mat
    try
        let mutable i: int = 0
        while i < (Seq.length (mat)) do
            let mutable line: string = ""
            let mutable j: int = 0
            while j < (Seq.length (_idx mat (i))) do
                line <- line + (_str (_idx (_idx mat (i)) (j)))
                if j < ((Seq.length (_idx mat (i))) - 1) then
                    line <- line + " "
                j <- j + 1
            printfn "%s" (line)
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let rgb_example: int array array array = [|[|[|127; 255; 0|]|]|]
        print_float_matrix (rgb_to_gray (rgb_example))
        let gray_example: float array array = [|[|26.0; 255.0; 14.0|]; [|5.0; 147.0; 20.0|]; [|1.0; 200.0; 0.0|]|]
        print_int_matrix (gray_to_binary (gray_example))
        let binary_image: int array array = [|[|0; 1; 0|]; [|0; 1; 0|]; [|0; 1; 0|]|]
        let kernel: int array array = [|[|0; 1; 0|]; [|1; 1; 1|]; [|0; 1; 0|]|]
        print_int_matrix (dilation (binary_image) (kernel))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
