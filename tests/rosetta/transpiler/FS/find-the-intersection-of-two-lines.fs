// Generated 2025-07-30 21:05 +0700

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
    x: float
    y: float
}
type Line = {
    slope: float
    yint: float
}
let rec createLine (a: Point) (b: Point) =
    let mutable __ret : Line = Unchecked.defaultof<Line>
    let mutable a = a
    let mutable b = b
    try
        let slope: float = ((b.y) - (a.y)) / ((b.x) - (a.x))
        let yint: float = (a.y) - (slope * (a.x))
        __ret <- { slope = slope; yint = yint }
        raise Return
        __ret
    with
        | Return -> __ret
and evalX (l: Line) (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable l = l
    let mutable x = x
    try
        __ret <- ((l.slope) * x) + (l.yint)
        raise Return
        __ret
    with
        | Return -> __ret
and intersection (l1: Line) (l2: Line) =
    let mutable __ret : Point = Unchecked.defaultof<Point>
    let mutable l1 = l1
    let mutable l2 = l2
    try
        if (l1.slope) = (l2.slope) then
            __ret <- { x = 0.0; y = 0.0 }
            raise Return
        let x: float = ((l2.yint) - (l1.yint)) / ((l1.slope) - (l2.slope))
        let y: float = evalX l1 x
        __ret <- { x = x; y = y }
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let l1: Line = createLine { x = 4.0; y = 0.0 } { x = 6.0; y = 10.0 }
        let l2: Line = createLine { x = 0.0; y = 3.0 } { x = 10.0; y = 7.0 }
        let p: Point = intersection l1 l2
        printfn "%s" (((("{" + (string (p.x))) + " ") + (string (p.y))) + "}")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
