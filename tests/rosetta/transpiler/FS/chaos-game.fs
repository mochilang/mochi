// Generated 2025-07-27 23:45 +0700

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
let width: int = 60
let height: int = unbox<int> ((unbox<float> width) * 0.86602540378)
let iterations: int = 5000
let mutable grid: string array array = [||]
let mutable y: int = 0
while y < height do
    let mutable line: string array = [||]
    let mutable x: int = 0
    while x < width do
        line <- unbox<string array> (Array.append line [|" "|])
        x <- x + 1
    grid <- unbox<string array array> (Array.append grid [|line|])
    y <- y + 1
let rec randInt (s: int) (n: int) =
    let mutable __ret : int array = Unchecked.defaultof<int array>
    let mutable s = s
    let mutable n = n
    try
        let next: int = ((((s * 1664525) + 1013904223) % 2147483647 + 2147483647) % 2147483647)
        __ret <- unbox<int array> [|next; ((next % n + n) % n)|]
        raise Return
        __ret
    with
        | Return -> __ret
let mutable seed: int = 1
let vertices: int array array = [|[|0; height - 1|]; [|width - 1; height - 1|]; [|unbox<int> (width / 2); 0|]|]
let mutable px: int = unbox<int> (width / 2)
let mutable py: int = unbox<int> (height / 2)
let mutable i: int = 0
while i < iterations do
    let mutable r: int array = randInt seed 3
    seed <- unbox<int> (r.[0])
    let idx: int = unbox<int> (r.[1])
    let v: int array = vertices.[idx]
    px <- unbox<int> ((unbox<int> (px + (unbox<int> (v.[0])))) / 2)
    py <- unbox<int> ((unbox<int> (py + (unbox<int> (v.[1])))) / 2)
    if (((px >= 0) && (px < width)) && (py >= 0)) && (py < height) then
        (grid.[py]).[px] <- "*"
    i <- i + 1
y <- 0
while y < height do
    let mutable line: string = ""
    let mutable x: int = 0
    while x < width do
        line <- line + (unbox<string> ((grid.[y]).[x]))
        x <- x + 1
    printfn "%s" line
    y <- y + 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
