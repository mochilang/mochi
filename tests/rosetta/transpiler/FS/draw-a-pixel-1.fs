// Generated 2025-07-28 10:03 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let width: int = 320
let height: int = 240
let mutable img: string array array = [||]
let mutable y: int = 0
while y < height do
    let mutable row: string array = [||]
    let mutable x: int = 0
    while x < width do
        row <- Array.append row [|"green"|]
        x <- x + 1
    img <- Array.append img [|row|]
    y <- y + 1
(img.[100]).[100] <- "red"
printfn "%s" (("The color of the pixel at (  0,   0) is " + ((img.[0]).[0])) + ".")
printfn "%s" (("The color of the pixel at (100, 100) is " + ((img.[100]).[100])) + ".")
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
