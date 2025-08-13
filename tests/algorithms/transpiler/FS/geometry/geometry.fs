// Generated 2025-08-13 16:01 +0700

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
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
type Angle = {
    mutable degrees: float
}
type Side = {
    mutable length: float
    mutable angle: Angle
    mutable next: int
}
type Ellipse = {
    mutable major: float
    mutable minor: float
}
type Circle = {
    mutable radius: float
}
type Polygon = {
    mutable sides: Side array
}
type Rectangle = {
    mutable short_side: Side
    mutable long_side: Side
    mutable poly: Polygon
}
type Square = {
    mutable side: Side
    mutable rect: Rectangle
}
let PI: float = 3.141592653589793
let rec make_angle (deg: float) =
    let mutable __ret : Angle = Unchecked.defaultof<Angle>
    let mutable deg = deg
    try
        if (deg < 0.0) || (deg > 360.0) then
            ignore (failwith ("degrees must be between 0 and 360"))
        __ret <- { degrees = deg }
        raise Return
        __ret
    with
        | Return -> __ret
and make_side (length: float) (angle: Angle) =
    let mutable __ret : Side = Unchecked.defaultof<Side>
    let mutable length = length
    let mutable angle = angle
    try
        if length <= 0.0 then
            ignore (failwith ("length must be positive"))
        __ret <- { length = length; angle = angle; next = -1 }
        raise Return
        __ret
    with
        | Return -> __ret
and ellipse_area (e: Ellipse) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable e = e
    try
        __ret <- (PI * (e.major)) * (e.minor)
        raise Return
        __ret
    with
        | Return -> __ret
and ellipse_perimeter (e: Ellipse) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable e = e
    try
        __ret <- PI * ((e.major) + (e.minor))
        raise Return
        __ret
    with
        | Return -> __ret
and circle_area (c: Circle) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable c = c
    try
        let e: Ellipse = { major = c.radius; minor = c.radius }
        let area: float = ellipse_area (e)
        __ret <- area
        raise Return
        __ret
    with
        | Return -> __ret
and circle_perimeter (c: Circle) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable c = c
    try
        let e: Ellipse = { major = c.radius; minor = c.radius }
        let per: float = ellipse_perimeter (e)
        __ret <- per
        raise Return
        __ret
    with
        | Return -> __ret
and circle_diameter (c: Circle) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable c = c
    try
        __ret <- (c.radius) * 2.0
        raise Return
        __ret
    with
        | Return -> __ret
and circle_max_parts (num_cuts: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable num_cuts = num_cuts
    try
        if num_cuts < 0.0 then
            ignore (failwith ("num_cuts must be positive"))
        __ret <- ((num_cuts + 2.0) + (num_cuts * num_cuts)) * 0.5
        raise Return
        __ret
    with
        | Return -> __ret
and make_polygon () =
    let mutable __ret : Polygon = Unchecked.defaultof<Polygon>
    try
        let mutable s: Side array = Array.empty<Side>
        __ret <- { sides = s }
        raise Return
        __ret
    with
        | Return -> __ret
and polygon_add_side (p: Polygon) (s: Side) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable p = p
    let mutable s = s
    try
        p.sides <- Array.append (p.sides) [|s|]
        __ret
    with
        | Return -> __ret
and polygon_get_side (p: Polygon) (index: int) =
    let mutable __ret : Side = Unchecked.defaultof<Side>
    let mutable p = p
    let mutable index = index
    try
        __ret <- _idx (p.sides) (int index)
        raise Return
        __ret
    with
        | Return -> __ret
and polygon_set_side (p: Polygon) (index: int) (s: Side) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable p = p
    let mutable index = index
    let mutable s = s
    try
        let mutable tmp: Side array = p.sides
        tmp.[index] <- s
        p.sides <- tmp
        __ret
    with
        | Return -> __ret
and make_rectangle (short_len: float) (long_len: float) =
    let mutable __ret : Rectangle = Unchecked.defaultof<Rectangle>
    let mutable short_len = short_len
    let mutable long_len = long_len
    try
        if (short_len <= 0.0) || (long_len <= 0.0) then
            ignore (failwith ("length must be positive"))
        let short: Side = make_side (short_len) (make_angle (90.0))
        let long: Side = make_side (long_len) (make_angle (90.0))
        let mutable p: Polygon = make_polygon()
        polygon_add_side (p) (short)
        polygon_add_side (p) (long)
        __ret <- { short_side = short; long_side = long; poly = p }
        raise Return
        __ret
    with
        | Return -> __ret
and rectangle_perimeter (r: Rectangle) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    try
        __ret <- (((r.short_side).length) + ((r.long_side).length)) * 2.0
        raise Return
        __ret
    with
        | Return -> __ret
and rectangle_area (r: Rectangle) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    try
        __ret <- ((r.short_side).length) * ((r.long_side).length)
        raise Return
        __ret
    with
        | Return -> __ret
and make_square (side_len: float) =
    let mutable __ret : Square = Unchecked.defaultof<Square>
    let mutable side_len = side_len
    try
        let rect: Rectangle = make_rectangle (side_len) (side_len)
        __ret <- { side = rect.short_side; rect = rect }
        raise Return
        __ret
    with
        | Return -> __ret
and square_perimeter (s: Square) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable s = s
    try
        let p: float = rectangle_perimeter (s.rect)
        __ret <- p
        raise Return
        __ret
    with
        | Return -> __ret
and square_area (s: Square) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable s = s
    try
        let a: float = rectangle_area (s.rect)
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let a: Angle = make_angle (90.0)
        ignore (printfn "%A" (a.degrees))
        let s: Side = make_side (5.0) (a)
        ignore (printfn "%A" (s.length))
        let e: Ellipse = { major = 5.0; minor = 10.0 }
        ignore (printfn "%A" (ellipse_area (e)))
        ignore (printfn "%A" (ellipse_perimeter (e)))
        let c: Circle = { radius = 5.0 }
        ignore (printfn "%A" (circle_area (c)))
        ignore (printfn "%A" (circle_perimeter (c)))
        ignore (printfn "%A" (circle_diameter (c)))
        ignore (printfn "%A" (circle_max_parts (7.0)))
        let r: Rectangle = make_rectangle (5.0) (10.0)
        ignore (printfn "%A" (rectangle_perimeter (r)))
        ignore (printfn "%A" (rectangle_area (r)))
        let q: Square = make_square (5.0)
        ignore (printfn "%A" (square_perimeter (q)))
        ignore (printfn "%A" (square_area (q)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
