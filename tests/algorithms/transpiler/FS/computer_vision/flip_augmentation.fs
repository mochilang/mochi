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
let rec flip_horizontal_image (img: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    try
        let mutable flipped: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (img)) do
            let mutable row: int array = _idx img (i)
            let mutable j: int = (Seq.length (row)) - 1
            let mutable new_row: int array = [||]
            while j >= 0 do
                new_row <- Array.append new_row [|_idx row (j)|]
                j <- j - 1
            flipped <- Array.append flipped [|new_row|]
            i <- i + 1
        __ret <- flipped
        raise Return
        __ret
    with
        | Return -> __ret
let rec flip_vertical_image (img: int array array) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    try
        let mutable flipped: int array array = [||]
        let mutable i: int = (Seq.length (img)) - 1
        while i >= 0 do
            flipped <- Array.append flipped [|_idx img (i)|]
            i <- i - 1
        __ret <- flipped
        raise Return
        __ret
    with
        | Return -> __ret
let rec flip_horizontal_boxes (boxes: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable boxes = boxes
    try
        let mutable result: float array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (boxes)) do
            let mutable b: float array = _idx boxes (i)
            let x_new: float = 1.0 - (_idx b (1))
            result <- Array.append result [|[|_idx b (0); x_new; _idx b (2); _idx b (3); _idx b (4)|]|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec flip_vertical_boxes (boxes: float array array) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable boxes = boxes
    try
        let mutable result: float array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (boxes)) do
            let mutable b: float array = _idx boxes (i)
            let y_new: float = 1.0 - (_idx b (2))
            result <- Array.append result [|[|_idx b (0); _idx b (1); y_new; _idx b (3); _idx b (4)|]|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec print_image (img: int array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable img = img
    try
        let mutable i: int = 0
        while i < (Seq.length (img)) do
            let mutable row: int array = _idx img (i)
            let mutable j: int = 0
            let mutable line: string = ""
            while j < (Seq.length (row)) do
                line <- (line + (_str (_idx row (j)))) + " "
                j <- j + 1
            printfn "%s" (line)
            i <- i + 1
        __ret
    with
        | Return -> __ret
let image: int array array = [|[|1; 2; 3|]; [|4; 5; 6|]; [|7; 8; 9|]|]
let boxes: float array array = [|[|0.0; 0.25; 0.25; 0.5; 0.5|]; [|1.0; 0.75; 0.75; 0.5; 0.5|]|]
printfn "%s" ("Original image:")
print_image (image)
printfn "%s" (_str (boxes))
printfn "%s" ("Horizontal flip:")
let h_img: int array array = flip_horizontal_image (image)
let h_boxes: float array array = flip_horizontal_boxes (boxes)
print_image (h_img)
printfn "%s" (_str (h_boxes))
printfn "%s" ("Vertical flip:")
let v_img: int array array = flip_vertical_image (image)
let v_boxes: float array array = flip_vertical_boxes (boxes)
print_image (v_img)
printfn "%s" (_str (v_boxes))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
