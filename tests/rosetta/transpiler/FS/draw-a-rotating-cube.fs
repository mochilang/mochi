// Generated 2025-07-28 10:03 +0700

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
type Point3 = {
    x: float
    y: float
    z: float
}
type Point2 = {
    x: int
    y: int
}
let PI: float = 3.141592653589793
let TWO_PI: float = 6.283185307179586
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
        let y: float = (float (_mod (x + PI) TWO_PI)) - PI
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
        let y: float = (float (_mod (x + PI) TWO_PI)) - PI
        let y2: float = y * y
        let y4: float = y2 * y2
        let y6: float = y4 * y2
        __ret <- ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
        raise Return
        __ret
    with
        | Return -> __ret
let nodes: Point3 array = [|{ x = -1.0; y = -1.0; z = -1.0 }; { x = -1.0; y = -1.0; z = 1.0 }; { x = -1.0; y = 1.0; z = -1.0 }; { x = -1.0; y = 1.0; z = 1.0 }; { x = 1.0; y = -1.0; z = -1.0 }; { x = 1.0; y = -1.0; z = 1.0 }; { x = 1.0; y = 1.0; z = -1.0 }; { x = 1.0; y = 1.0; z = 1.0 }|]
let edges: int array array = [|[|0; 1|]; [|1; 3|]; [|3; 2|]; [|2; 0|]; [|4; 5|]; [|5; 7|]; [|7; 6|]; [|6; 4|]; [|0; 4|]; [|1; 5|]; [|2; 6|]; [|3; 7|]|]
let rec rotate (p: Point3) (ax: float) (ay: float) =
    let mutable __ret : Point3 = Unchecked.defaultof<Point3>
    let mutable p = p
    let mutable ax = ax
    let mutable ay = ay
    try
        let sinx: float = _sin ax
        let cosx: float = _cos ax
        let siny: float = _sin ay
        let cosy: float = _cos ay
        let x1: float = p.x
        let y1: float = ((p.y) * cosx) - ((p.z) * sinx)
        let z1: float = ((p.y) * sinx) + ((p.z) * cosx)
        let x2: float = (x1 * cosy) + (z1 * siny)
        let z2: float = ((-x1) * siny) + (z1 * cosy)
        __ret <- { x = x2; y = y1; z = z2 }
        raise Return
        __ret
    with
        | Return -> __ret
let width: int = 40
let height: int = 20
let distance: float = 3.0
let scale: float = 8.0
let rec project (p: Point3) =
    let mutable __ret : Point2 = Unchecked.defaultof<Point2>
    let mutable p = p
    try
        let factor: float = scale / ((p.z) + distance)
        let x: int = (int ((p.x) * factor)) + (width / 2)
        let y: int = (int ((-(p.y)) * factor)) + (height / 2)
        __ret <- { x = x; y = y }
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
and drawPoint (g: string array array) (x: int) (y: int) (ch: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable g = g
    let mutable x = x
    let mutable y = y
    let mutable ch = ch
    try
        if (((x >= 0) && (x < width)) && (y >= 0)) && (y < height) then
            let mutable row: string array = g.[y]
            row.[x] <- ch
            g.[y] <- row
        __ret
    with
        | Return -> __ret
and bresenham (x0: int) (y0: int) (x1: int) (y1: int) (g: string array array) (ch: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable x0 = x0
    let mutable y0 = y0
    let mutable x1 = x1
    let mutable y1 = y1
    let mutable g = g
    let mutable ch = ch
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
                    drawPoint g x0 y0 ch
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
            out <- (out + line) + "\n"
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
        let mutable f: int = 0
        while f < 10 do
            let mutable grid: string array array = clearGrid()
            let mutable rot: Point2 array = [||]
            let mutable i: int = 0
            let mutable ay: float = (PI / 4.0) + (((float f) * PI) / 10.0)
            while i < (Seq.length nodes) do
                let p: Point3 = rotate (nodes.[i]) (PI / 4.0) ay
                let pp: Point2 = project p
                rot <- Array.append rot [|pp|]
                i <- i + 1
            let mutable e: int = 0
            while e < (Seq.length edges) do
                let a: int = (edges.[e]).[0]
                let b: int = (edges.[e]).[1]
                let p1: Point2 = rot.[a]
                let p2: Point2 = rot.[b]
                bresenham (p1.x) (p1.y) (p2.x) (p2.y) grid "#"
                e <- e + 1
            printfn "%s" (render grid)
            f <- f + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
