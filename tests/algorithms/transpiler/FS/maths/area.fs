// Generated 2025-08-08 17:07 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
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
let rec sin_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: float = (_mod (x + PI) (TWO_PI)) - PI
        let y2: float = y * y
        let y3: float = y2 * y
        let y5: float = y3 * y2
        let y7: float = y5 * y2
        __ret <- ((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec cos_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let y: float = (_mod (x + PI) (TWO_PI)) - PI
        let y2: float = y * y
        let y4: float = y2 * y2
        let y6: float = y4 * y2
        __ret <- ((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec tan_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (sin_approx (x)) / (cos_approx (x))
        raise Return
        __ret
    with
        | Return -> __ret
let rec sqrt_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec surface_area_cube (side_length: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable side_length = side_length
    try
        if side_length < 0.0 then
            printfn "%s" ("ValueError: surface_area_cube() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- (6.0 * side_length) * side_length
        raise Return
        __ret
    with
        | Return -> __ret
let rec surface_area_cuboid (length: float) (breadth: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable length = length
    let mutable breadth = breadth
    let mutable height = height
    try
        if ((length < 0.0) || (breadth < 0.0)) || (height < 0.0) then
            printfn "%s" ("ValueError: surface_area_cuboid() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- 2.0 * (((length * breadth) + (breadth * height)) + (length * height))
        raise Return
        __ret
    with
        | Return -> __ret
let rec surface_area_sphere (radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius = radius
    try
        if radius < 0.0 then
            printfn "%s" ("ValueError: surface_area_sphere() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- ((4.0 * PI) * radius) * radius
        raise Return
        __ret
    with
        | Return -> __ret
let rec surface_area_hemisphere (radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius = radius
    try
        if radius < 0.0 then
            printfn "%s" ("ValueError: surface_area_hemisphere() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- ((3.0 * PI) * radius) * radius
        raise Return
        __ret
    with
        | Return -> __ret
let rec surface_area_cone (radius: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius = radius
    let mutable height = height
    try
        if (radius < 0.0) || (height < 0.0) then
            printfn "%s" ("ValueError: surface_area_cone() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        let slant: float = sqrt_approx ((height * height) + (radius * radius))
        __ret <- (PI * radius) * (radius + slant)
        raise Return
        __ret
    with
        | Return -> __ret
let rec surface_area_conical_frustum (radius1: float) (radius2: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius1 = radius1
    let mutable radius2 = radius2
    let mutable height = height
    try
        if ((radius1 < 0.0) || (radius2 < 0.0)) || (height < 0.0) then
            printfn "%s" ("ValueError: surface_area_conical_frustum() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        let slant: float = sqrt_approx ((height * height) + ((radius1 - radius2) * (radius1 - radius2)))
        __ret <- PI * (((slant * (radius1 + radius2)) + (radius1 * radius1)) + (radius2 * radius2))
        raise Return
        __ret
    with
        | Return -> __ret
let rec surface_area_cylinder (radius: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius = radius
    let mutable height = height
    try
        if (radius < 0.0) || (height < 0.0) then
            printfn "%s" ("ValueError: surface_area_cylinder() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- ((2.0 * PI) * radius) * (height + radius)
        raise Return
        __ret
    with
        | Return -> __ret
let rec surface_area_torus (torus_radius: float) (tube_radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable torus_radius = torus_radius
    let mutable tube_radius = tube_radius
    try
        if (torus_radius < 0.0) || (tube_radius < 0.0) then
            printfn "%s" ("ValueError: surface_area_torus() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        if torus_radius < tube_radius then
            printfn "%s" ("ValueError: surface_area_torus() does not support spindle or self intersecting tori")
            __ret <- 0.0
            raise Return
        __ret <- (((4.0 * PI) * PI) * torus_radius) * tube_radius
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_rectangle (length: float) (width: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable length = length
    let mutable width = width
    try
        if (length < 0.0) || (width < 0.0) then
            printfn "%s" ("ValueError: area_rectangle() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- length * width
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_square (side_length: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable side_length = side_length
    try
        if side_length < 0.0 then
            printfn "%s" ("ValueError: area_square() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- side_length * side_length
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_triangle (``base``: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable height = height
    try
        if (``base`` < 0.0) || (height < 0.0) then
            printfn "%s" ("ValueError: area_triangle() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- (``base`` * height) / 2.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_triangle_three_sides (side1: float) (side2: float) (side3: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable side1 = side1
    let mutable side2 = side2
    let mutable side3 = side3
    try
        if ((side1 < 0.0) || (side2 < 0.0)) || (side3 < 0.0) then
            printfn "%s" ("ValueError: area_triangle_three_sides() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        if (((side1 + side2) < side3) || ((side1 + side3) < side2)) || ((side2 + side3) < side1) then
            printfn "%s" ("ValueError: Given three sides do not form a triangle")
            __ret <- 0.0
            raise Return
        let s: float = ((side1 + side2) + side3) / 2.0
        let prod: float = ((s * (s - side1)) * (s - side2)) * (s - side3)
        let res: float = sqrt_approx (prod)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_parallelogram (``base``: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable height = height
    try
        if (``base`` < 0.0) || (height < 0.0) then
            printfn "%s" ("ValueError: area_parallelogram() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- ``base`` * height
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_trapezium (base1: float) (base2: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable base1 = base1
    let mutable base2 = base2
    let mutable height = height
    try
        if ((base1 < 0.0) || (base2 < 0.0)) || (height < 0.0) then
            printfn "%s" ("ValueError: area_trapezium() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- (0.5 * (base1 + base2)) * height
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_circle (radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius = radius
    try
        if radius < 0.0 then
            printfn "%s" ("ValueError: area_circle() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- (PI * radius) * radius
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_ellipse (radius_x: float) (radius_y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius_x = radius_x
    let mutable radius_y = radius_y
    try
        if (radius_x < 0.0) || (radius_y < 0.0) then
            printfn "%s" ("ValueError: area_ellipse() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- (PI * radius_x) * radius_y
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_rhombus (diagonal1: float) (diagonal2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable diagonal1 = diagonal1
    let mutable diagonal2 = diagonal2
    try
        if (diagonal1 < 0.0) || (diagonal2 < 0.0) then
            printfn "%s" ("ValueError: area_rhombus() only accepts non-negative values")
            __ret <- 0.0
            raise Return
        __ret <- (0.5 * diagonal1) * diagonal2
        raise Return
        __ret
    with
        | Return -> __ret
let rec area_reg_polygon (sides: int) (length: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable sides = sides
    let mutable length = length
    try
        if sides < 3 then
            printfn "%s" ("ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides")
            __ret <- 0.0
            raise Return
        if length < 0.0 then
            printfn "%s" ("ValueError: area_reg_polygon() only accepts non-negative values as length of a side")
            __ret <- 0.0
            raise Return
        let n: float = float sides
        __ret <- ((n * length) * length) / (4.0 * (tan_approx (PI / n)))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("[DEMO] Areas of various geometric shapes:")
printfn "%s" ("Rectangle: " + (_str (area_rectangle (10.0) (20.0))))
printfn "%s" ("Square: " + (_str (area_square (10.0))))
printfn "%s" ("Triangle: " + (_str (area_triangle (10.0) (10.0))))
let TRI_THREE_SIDES: float = area_triangle_three_sides (5.0) (12.0) (13.0)
printfn "%s" ("Triangle Three Sides: " + (_str (TRI_THREE_SIDES)))
printfn "%s" ("Parallelogram: " + (_str (area_parallelogram (10.0) (20.0))))
printfn "%s" ("Rhombus: " + (_str (area_rhombus (10.0) (20.0))))
printfn "%s" ("Trapezium: " + (_str (area_trapezium (10.0) (20.0) (30.0))))
printfn "%s" ("Circle: " + (_str (area_circle (20.0))))
printfn "%s" ("Ellipse: " + (_str (area_ellipse (10.0) (20.0))))
printfn "%s" ("")
printfn "%s" ("Surface Areas of various geometric shapes:")
printfn "%s" ("Cube: " + (_str (surface_area_cube (20.0))))
printfn "%s" ("Cuboid: " + (_str (surface_area_cuboid (10.0) (20.0) (30.0))))
printfn "%s" ("Sphere: " + (_str (surface_area_sphere (20.0))))
printfn "%s" ("Hemisphere: " + (_str (surface_area_hemisphere (20.0))))
printfn "%s" ("Cone: " + (_str (surface_area_cone (10.0) (20.0))))
printfn "%s" ("Conical Frustum: " + (_str (surface_area_conical_frustum (10.0) (20.0) (30.0))))
printfn "%s" ("Cylinder: " + (_str (surface_area_cylinder (10.0) (20.0))))
printfn "%s" ("Torus: " + (_str (surface_area_torus (20.0) (10.0))))
printfn "%s" ("Equilateral Triangle: " + (_str (area_reg_polygon (3) (10.0))))
printfn "%s" ("Square: " + (_str (area_reg_polygon (4) (10.0))))
printfn "%s" ("Regular Pentagon: " + (_str (area_reg_polygon (5) (10.0))))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
