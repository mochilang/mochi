// Generated 2025-08-01 19:34 +0700

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
let PI: float = 3.141592653589793
let width: int = 80
let height: int = 40
let depth: int = 6
let angle: float = 12.0
let length: float = 12.0
let frac: float = 0.8
let rec _mod (x: float) (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable m = m
    try
        __ret <- x - ((float (int (x / m))) * m)
        raise Return
        __ret
    with
        | Return -> __ret
and _sin (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = (float (_mod (x + PI) (2.0 * PI))) - PI
        let y2: float = y * y
        let y3: float = y2 * y
        let y5: float = y3 * y2
        let y7: float = y5 * y2
        __ret <- ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
        raise Return
        __ret
    with
        | Return -> __ret
and _cos (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = (float (_mod (x + PI) (2.0 * PI))) - PI
        let y2: float = y * y
        let y4: float = y2 * y2
        let y6: float = y4 * y2
        __ret <- ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
        raise Return
        __ret
    with
        | Return -> __ret
and clearGrid () =
    let mutable __ret : string array array = Unchecked.defaultof<string array array>
    try
        let mutable g: string array array = [||]
        let mutable y: int = 0
        while y < height do
            let mutable row: string array = [||]
            let mutable x: int = 0
            while x < width do
                row <- Array.append row [|" "|]
                x <- x + 1
            g <- Array.append g [|row|]
            y <- y + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and drawPoint (g: string array array) (x: int) (y: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable x = x
    let mutable y = y
    try
        if (((x >= 0) && (x < width)) && (y >= 0)) && (y < height) then
            let mutable row: string array = g.[y]
            row.[x] <- "#"
            g.[y] <- row
        __ret
    with
        | Return -> __ret
and bresenham (x0: int) (y0: int) (x1: int) (y1: int) (g: string array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x0 = x0
    let mutable y0 = y0
    let mutable x1 = x1
    let mutable y1 = y1
    let mutable g = g
    try
        let mutable dx: int = x1 - x0
        if dx < 0 then
            dx <- -dx
        let mutable dy: int = y1 - y0
        if dy < 0 then
            dy <- -dy
        let mutable sx: int = -1
        if x0 < x1 then
            sx <- 1
        let mutable sy: int = -1
        if y0 < y1 then
            sy <- 1
        let mutable err: int = dx - dy
        try
            while true do
                try
                    drawPoint g x0 y0
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
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        __ret
    with
        | Return -> __ret
and ftree (g: string array array) (x: float) (y: float) (dist: float) (dir: float) (d: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable x = x
    let mutable y = y
    let mutable dist = dist
    let mutable dir = dir
    let mutable d = d
    try
        let rad: float = (dir * PI) / 180.0
        let x2: float = x + (float (dist * (float (_sin rad))))
        let y2: float = y - (float (dist * (float (_cos rad))))
        bresenham (int x) (int y) (int x2) (int y2) g
        if d > 0 then
            ftree g x2 y2 (dist * frac) (dir - angle) (d - 1)
            ftree g x2 y2 (dist * frac) (dir + angle) (d - 1)
        __ret
    with
        | Return -> __ret
and render (g: string array array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable g = g
    try
        let mutable out: string = ""
        let mutable y: int = 0
        while y < height do
            let mutable line: string = ""
            let mutable x: int = 0
            while x < width do
                line <- line + ((g.[y]).[x])
                x <- x + 1
            out <- out + line
            if y < (height - 1) then
                out <- out + "\n"
            y <- y + 1
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable grid: string array array = clearGrid()
        ftree grid (float (width / 2)) (float (height - 1)) length 0.0 depth
        printfn "%s" (render grid)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
