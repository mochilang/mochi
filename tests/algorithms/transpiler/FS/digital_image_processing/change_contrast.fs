// Generated 2025-08-07 14:57 +0700

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
let rec change_contrast (img: int array array) (level: int) =
    let mutable __ret : int array array = Unchecked.defaultof<int array array>
    let mutable img = img
    let mutable level = level
    try
        let factor: float = (259.0 * ((float level) + 255.0)) / (255.0 * (259.0 - (float level)))
        let mutable result: int array array = [||]
        let mutable i: int = 0
        while i < (Seq.length (img)) do
            let mutable row: int array = _idx img (i)
            let mutable new_row: int array = [||]
            let mutable j: int = 0
            while j < (Seq.length (row)) do
                let c: int = _idx row (j)
                let contrasted: int = int (128.0 + (factor * ((float c) - 128.0)))
                let clamped: int = if contrasted < 0 then 0 else (if contrasted > 255 then 255 else contrasted)
                new_row <- Array.append new_row [|clamped|]
                j <- j + 1
            result <- Array.append result [|new_row|]
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
let image: int array array = [|[|100; 125; 150|]; [|175; 200; 225|]; [|50; 75; 100|]|]
printfn "%s" ("Original image:")
print_image (image)
let contrasted: int array array = change_contrast (image) (170)
printfn "%s" ("After contrast:")
print_image (contrasted)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
