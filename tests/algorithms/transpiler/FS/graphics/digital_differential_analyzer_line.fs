// Generated 2025-08-13 16:13 +0700

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
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
type Point = {
    mutable _x: int
    mutable _y: int
}
let rec abs_int (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        __ret <- if n < 0 then (-n) else n
        raise Return
        __ret
    with
        | Return -> __ret
and round_int (_x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable _x = _x
    try
        __ret <- int (_x + 0.5)
        raise Return
        __ret
    with
        | Return -> __ret
and digital_differential_analyzer_line (p1: Point) (p2: Point) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable p1 = p1
    let mutable p2 = p2
    try
        let dx: int = (p2._x) - (p1._x)
        let dy: int = (p2._y) - (p1._y)
        let abs_dx: int = abs_int (dx)
        let abs_dy: int = abs_int (dy)
        let steps: int = if abs_dx > abs_dy then abs_dx else abs_dy
        let x_increment: float = (float dx) / (float steps)
        let y_increment: float = (float dy) / (float steps)
        let mutable coordinates: Point array = Array.empty<Point>
        let mutable _x: float = float (p1._x)
        let mutable _y: float = float (p1._y)
        let mutable i: int = 0
        while i < steps do
            _x <- _x + x_increment
            _y <- _y + y_increment
            let point: Point = { _x = round_int (_x); _y = round_int (_y) }
            coordinates <- Array.append coordinates [|point|]
            i <- i + 1
        __ret <- coordinates
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let result: Point array = digital_differential_analyzer_line ({ _x = 1; _y = 1 }) ({ _x = 4; _y = 4 })
        ignore (printfn "%s" (_repr (result)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
