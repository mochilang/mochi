// Generated 2025-07-28 10:03 +0700

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
let rec repeat (ch: string) (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ch = ch
    let mutable n = n
    try
        let mutable s: string = ""
        let mutable i: int = 0
        while i < n do
            s <- s + ch
            i <- i + 1
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let rec cubLine (n: int) (dx: int) (dy: int) (cde: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable dx = dx
    let mutable dy = dy
    let mutable cde = cde
    try
        let mutable line: string = (unbox<string> (repeat " " (n + 1))) + (cde.Substring(0, 1 - 0))
        let mutable d: int = (9 * dx) - 1
        while d > 0 do
            line <- line + (cde.Substring(1, 2 - 1))
            d <- d - 1
        line <- line + (cde.Substring(0, 1 - 0))
        line <- (line + (unbox<string> (repeat " " dy))) + (cde.Substring(2, (String.length cde) - 2))
        printfn "%s" line
        __ret
    with
        | Return -> __ret
let rec cuboid (dx: int) (dy: int) (dz: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable dx = dx
    let mutable dy = dy
    let mutable dz = dz
    try
        printfn "%s" (((((("cuboid " + (string dx)) + " ") + (string dy)) + " ") + (string dz)) + ":")
        cubLine (dy + 1) dx 0 "+-"
        let mutable i: int = 1
        while i <= dy do
            cubLine ((dy - i) + 1) dx (i - 1) "/ |"
            i <- i + 1
        cubLine 0 dx dy "+-|"
        let mutable j: int = ((4 * dz) - dy) - 2
        while j > 0 do
            cubLine 0 dx dy "| |"
            j <- j - 1
        cubLine 0 dx dy "| +"
        i <- 1
        while i <= dy do
            cubLine 0 dx (dy - i) "| /"
            i <- i + 1
        cubLine 0 dx 0 "+-\n"
        __ret
    with
        | Return -> __ret
cuboid 2 3 4
printfn "%s" ""
cuboid 1 1 1
printfn "%s" ""
cuboid 6 2 1
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
