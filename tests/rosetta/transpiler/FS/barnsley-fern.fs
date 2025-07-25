// Generated 2025-07-26 04:38 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let xMin: float = -2.182
let xMax: float = 2.6558
let yMin: float = 0.0
let yMax: float = 9.9983
let width: int = 60
let nIter: int = 10000
let dx: float = xMax - xMin
let dy: float = yMax - yMin
let height: int = int (((float width) * dy) / dx)
let mutable grid: string array array = [||]
let mutable row: int = 0
while row < height do
    let mutable line: string array = [||]
    let mutable col: int = 0
    while col < width do
        line <- Array.append line [|" "|]
        col <- col + 1
    grid <- Array.append grid [|line|]
    row <- row + 1
let mutable seed: int = 1
let rec randInt (s: int) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    let mutable n = n
    try
        let next: int = ((((s * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        __ret <- [|next; ((next % n + n) % n)|]
        raise Return
        __ret
    with
        | Return -> __ret
let mutable x: float = 0.0
let mutable y: float = 0.0
let mutable ix: int = int (((float width) * (x - xMin)) / dx)
let mutable iy: int = int (((float height) * (yMax - y)) / dy)
if (((ix >= 0) && (ix < width)) && (iy >= 0)) && (iy < height) then
    (grid.[iy]).[ix] <- "*"
let mutable i: int = 0
while i < nIter do
    let mutable res: int array = randInt seed 100
    seed <- res.[0]
    let r: int = res.[1]
    if r < 85 then
        let nx: float = (0.85 * x) + (0.04 * y)
        let ny: float = (((-0.04) * x) + (0.85 * y)) + 1.6
        x <- nx
        y <- ny
    else
        if r < 92 then
            let nx: float = (0.2 * x) - (0.26 * y)
            let ny: float = ((0.23 * x) + (0.22 * y)) + 1.6
            x <- nx
            y <- ny
        else
            if r < 99 then
                let nx: float = ((-0.15) * x) + (0.28 * y)
                let ny: float = ((0.26 * x) + (0.24 * y)) + 0.44
                x <- nx
                y <- ny
            else
                x <- 0.0
                y <- 0.16 * y
    ix <- int (((float width) * (x - xMin)) / dx)
    iy <- int (((float height) * (yMax - y)) / dy)
    if (((ix >= 0) && (ix < width)) && (iy >= 0)) && (iy < height) then
        (grid.[iy]).[ix] <- "*"
    i <- i + 1
row <- 0
while row < height do
    let mutable line: string = ""
    let mutable col: int = 0
    while col < width do
        line <- line + (unbox<string> ((grid.[row]).[col]))
        col <- col + 1
    printfn "%s" line
    row <- row + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
