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
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let rec get_greyscale (blue: int) (green: int) (red: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable blue = blue
    let mutable green = green
    let mutable red = red
    try
        let b: float = float blue
        let g: float = float green
        let r: float = float red
        __ret <- int (((0.114 * b) + (0.587 * g)) + (0.299 * r))
        raise Return
        __ret
    with
        | Return -> __ret
and zeros (h: int) (w: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable h = h
    let mutable w = w
    try
        let mutable table: int array array = [||]
        let mutable i: int = 0
        while i < h do
            let mutable row: int array = [||]
            let mutable j: int = 0
            while j < w do
                row <- Array.append row [|0|]
                j <- j + 1
            table <- Array.append table [|row|]
            i <- i + 1
        __ret <- table
        raise Return
        __ret
    with
        | Return -> __ret
and burkes_dither (img: int array array array) (threshold: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    let mutable threshold = threshold
    try
        let height: int = Seq.length (img)
        let width: int = Seq.length (_idx img (0))
        let mutable error_table: int array array = zeros (height + 1) (width + 4)
        let mutable output: int array array = [||]
        let mutable y: int = 0
        while y < height do
            let mutable row: int array = [||]
            let mutable x: int = 0
            while x < width do
                let px: int array = _idx (_idx img (y)) (x)
                let grey: int = get_greyscale (_idx px (0)) (_idx px (1)) (_idx px (2))
                let total: int = grey + (_idx (_idx error_table (y)) (x + 2))
                let mutable new_val: int = 0
                let mutable current_error: int = 0
                if threshold > total then
                    new_val <- 0
                    current_error <- total
                else
                    new_val <- 255
                    current_error <- total - 255
                row <- Array.append row [|new_val|]
                error_table.[y].[x + 3] <- (_idx (_idx error_table (y)) (x + 3)) + ((8 * current_error) / 32)
                error_table.[y].[x + 4] <- (_idx (_idx error_table (y)) (x + 4)) + ((4 * current_error) / 32)
                error_table.[y + 1].[x + 2] <- (_idx (_idx error_table (y + 1)) (x + 2)) + ((8 * current_error) / 32)
                error_table.[y + 1].[x + 3] <- (_idx (_idx error_table (y + 1)) (x + 3)) + ((4 * current_error) / 32)
                error_table.[y + 1].[x + 4] <- (_idx (_idx error_table (y + 1)) (x + 4)) + ((2 * current_error) / 32)
                error_table.[y + 1].[x + 1] <- (_idx (_idx error_table (y + 1)) (x + 1)) + ((4 * current_error) / 32)
                error_table.[y + 1].[x] <- (_idx (_idx error_table (y + 1)) (x)) + ((2 * current_error) / 32)
                x <- x + 1
            output <- Array.append output [|row|]
            y <- y + 1
        __ret <- output
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let img: int array array array = [|[|[|0; 0; 0|]; [|64; 64; 64|]; [|128; 128; 128|]; [|192; 192; 192|]|]; [|[|255; 255; 255|]; [|200; 200; 200|]; [|150; 150; 150|]; [|100; 100; 100|]|]; [|[|30; 144; 255|]; [|255; 0; 0|]; [|0; 255; 0|]; [|0; 0; 255|]|]; [|[|50; 100; 150|]; [|80; 160; 240|]; [|70; 140; 210|]; [|60; 120; 180|]|]|]
        let result: int array array = burkes_dither (img) (128)
        let mutable y: int = 0
        while y < (Seq.length (result)) do
            let mutable line: string = ""
            let mutable x: int = 0
            while x < (Seq.length (_idx result (y))) do
                line <- line + (_str (_idx (_idx result (y)) (x)))
                if x < ((Seq.length (_idx result (y))) - 1) then
                    line <- line + " "
                x <- x + 1
            printfn "%s" (line)
            y <- y + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
