// Generated 2025-07-26 03:08 +0000

exception Break
exception Continue

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
type Point = {
    x: int
    y: int
}
let rec absi (x: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        __ret <- if x < 0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and bresenham (x0: int) (y0: int) (x1: int) (y1: int) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable x0 = x0
    let mutable y0 = y0
    let mutable x1 = x1
    let mutable y1 = y1
    try
        let mutable dx: int = absi (x1 - x0)
        let mutable dy: int = absi (y1 - y0)
        let mutable sx: int = -1
        if x0 < x1 then
            sx <- 1
        let mutable sy: int = -1
        if y0 < y1 then
            sy <- 1
        let mutable err: int = dx - dy
        let mutable pts: Point array = [||]
        try
            while true do
                pts <- unbox<Point array> (Array.append pts [|{ x = x0; y = y0 }|])
                if (x0 = x1) && (y0 = y1) then
                    raise Break
                let mutable e2: int = 2 * err
                if e2 > (-dy) then
                    err <- err - dy
                    x0 <- x0 + sx
                if e2 < dx then
                    err <- err + dx
                    y0 <- y0 + sy
        with
        | Break -> ()
        | Continue -> ()
        __ret <- pts
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let pts: Point array = bresenham 0 0 6 4
        let mutable i: int = 0
        while i < (unbox<int> (Array.length pts)) do
            let p: Point = pts.[i]
            printfn "%s" (((("(" + (string (p.x))) + ",") + (string (p.y))) + ")")
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
