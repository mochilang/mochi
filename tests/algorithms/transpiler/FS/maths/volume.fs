// Generated 2025-08-08 18:58 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let PI: float = 3.141592653589793
let SQRT5: float = 2.23606797749979
let rec minf (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and maxf (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and vol_cube (side_length: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable side_length = side_length
    try
        if side_length < 0.0 then
            failwith ("vol_cube() only accepts non-negative values")
        __ret <- (side_length * side_length) * side_length
        raise Return
        __ret
    with
        | Return -> __ret
and vol_spherical_cap (height: float) (radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable height = height
    let mutable radius = radius
    try
        if (height < 0.0) || (radius < 0.0) then
            failwith ("vol_spherical_cap() only accepts non-negative values")
        __ret <- ((((1.0 / 3.0) * PI) * height) * height) * ((3.0 * radius) - height)
        raise Return
        __ret
    with
        | Return -> __ret
and vol_sphere (radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius = radius
    try
        if radius < 0.0 then
            failwith ("vol_sphere() only accepts non-negative values")
        __ret <- ((((4.0 / 3.0) * PI) * radius) * radius) * radius
        raise Return
        __ret
    with
        | Return -> __ret
and vol_spheres_intersect (radius_1: float) (radius_2: float) (centers_distance: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius_1 = radius_1
    let mutable radius_2 = radius_2
    let mutable centers_distance = centers_distance
    try
        if ((radius_1 < 0.0) || (radius_2 < 0.0)) || (centers_distance < 0.0) then
            failwith ("vol_spheres_intersect() only accepts non-negative values")
        if centers_distance = 0.0 then
            __ret <- vol_sphere (minf (radius_1) (radius_2))
            raise Return
        let h1: float = (((radius_1 - radius_2) + centers_distance) * ((radius_1 + radius_2) - centers_distance)) / (2.0 * centers_distance)
        let h2: float = (((radius_2 - radius_1) + centers_distance) * ((radius_2 + radius_1) - centers_distance)) / (2.0 * centers_distance)
        __ret <- (vol_spherical_cap (h1) (radius_2)) + (vol_spherical_cap (h2) (radius_1))
        raise Return
        __ret
    with
        | Return -> __ret
and vol_spheres_union (radius_1: float) (radius_2: float) (centers_distance: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius_1 = radius_1
    let mutable radius_2 = radius_2
    let mutable centers_distance = centers_distance
    try
        if ((radius_1 <= 0.0) || (radius_2 <= 0.0)) || (centers_distance < 0.0) then
            failwith ("vol_spheres_union() only accepts non-negative values, non-zero radius")
        if centers_distance = 0.0 then
            __ret <- vol_sphere (maxf (radius_1) (radius_2))
            raise Return
        __ret <- ((vol_sphere (radius_1)) + (vol_sphere (radius_2))) - (vol_spheres_intersect (radius_1) (radius_2) (centers_distance))
        raise Return
        __ret
    with
        | Return -> __ret
and vol_cuboid (width: float) (height: float) (length: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable width = width
    let mutable height = height
    let mutable length = length
    try
        if ((width < 0.0) || (height < 0.0)) || (length < 0.0) then
            failwith ("vol_cuboid() only accepts non-negative values")
        __ret <- (width * height) * length
        raise Return
        __ret
    with
        | Return -> __ret
and vol_cone (area_of_base: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable area_of_base = area_of_base
    let mutable height = height
    try
        if (height < 0.0) || (area_of_base < 0.0) then
            failwith ("vol_cone() only accepts non-negative values")
        __ret <- (area_of_base * height) / 3.0
        raise Return
        __ret
    with
        | Return -> __ret
and vol_right_circ_cone (radius: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius = radius
    let mutable height = height
    try
        if (height < 0.0) || (radius < 0.0) then
            failwith ("vol_right_circ_cone() only accepts non-negative values")
        __ret <- (((PI * radius) * radius) * height) / 3.0
        raise Return
        __ret
    with
        | Return -> __ret
and vol_prism (area_of_base: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable area_of_base = area_of_base
    let mutable height = height
    try
        if (height < 0.0) || (area_of_base < 0.0) then
            failwith ("vol_prism() only accepts non-negative values")
        __ret <- area_of_base * height
        raise Return
        __ret
    with
        | Return -> __ret
and vol_pyramid (area_of_base: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable area_of_base = area_of_base
    let mutable height = height
    try
        if (height < 0.0) || (area_of_base < 0.0) then
            failwith ("vol_pyramid() only accepts non-negative values")
        __ret <- (area_of_base * height) / 3.0
        raise Return
        __ret
    with
        | Return -> __ret
and vol_hemisphere (radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius = radius
    try
        if radius < 0.0 then
            failwith ("vol_hemisphere() only accepts non-negative values")
        __ret <- ((((radius * radius) * radius) * PI) * 2.0) / 3.0
        raise Return
        __ret
    with
        | Return -> __ret
and vol_circular_cylinder (radius: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable radius = radius
    let mutable height = height
    try
        if (height < 0.0) || (radius < 0.0) then
            failwith ("vol_circular_cylinder() only accepts non-negative values")
        __ret <- ((radius * radius) * height) * PI
        raise Return
        __ret
    with
        | Return -> __ret
and vol_hollow_circular_cylinder (inner_radius: float) (outer_radius: float) (height: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable inner_radius = inner_radius
    let mutable outer_radius = outer_radius
    let mutable height = height
    try
        if ((inner_radius < 0.0) || (outer_radius < 0.0)) || (height < 0.0) then
            failwith ("vol_hollow_circular_cylinder() only accepts non-negative values")
        if outer_radius <= inner_radius then
            failwith ("outer_radius must be greater than inner_radius")
        __ret <- (PI * ((outer_radius * outer_radius) - (inner_radius * inner_radius))) * height
        raise Return
        __ret
    with
        | Return -> __ret
and vol_conical_frustum (height: float) (radius_1: float) (radius_2: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable height = height
    let mutable radius_1 = radius_1
    let mutable radius_2 = radius_2
    try
        if ((radius_1 < 0.0) || (radius_2 < 0.0)) || (height < 0.0) then
            failwith ("vol_conical_frustum() only accepts non-negative values")
        __ret <- (((1.0 / 3.0) * PI) * height) * (((radius_1 * radius_1) + (radius_2 * radius_2)) + (radius_1 * radius_2))
        raise Return
        __ret
    with
        | Return -> __ret
and vol_torus (torus_radius: float) (tube_radius: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable torus_radius = torus_radius
    let mutable tube_radius = tube_radius
    try
        if (torus_radius < 0.0) || (tube_radius < 0.0) then
            failwith ("vol_torus() only accepts non-negative values")
        __ret <- ((((2.0 * PI) * PI) * torus_radius) * tube_radius) * tube_radius
        raise Return
        __ret
    with
        | Return -> __ret
and vol_icosahedron (tri_side: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable tri_side = tri_side
    try
        if tri_side < 0.0 then
            failwith ("vol_icosahedron() only accepts non-negative values")
        __ret <- ((((tri_side * tri_side) * tri_side) * (3.0 + SQRT5)) * 5.0) / 12.0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" ("Volumes:")
        printfn "%s" ("Cube: " + (_str (vol_cube (2.0))))
        printfn "%s" ("Cuboid: " + (_str (vol_cuboid (2.0) (2.0) (2.0))))
        printfn "%s" ("Cone: " + (_str (vol_cone (2.0) (2.0))))
        printfn "%s" ("Right Circular Cone: " + (_str (vol_right_circ_cone (2.0) (2.0))))
        printfn "%s" ("Prism: " + (_str (vol_prism (2.0) (2.0))))
        printfn "%s" ("Pyramid: " + (_str (vol_pyramid (2.0) (2.0))))
        printfn "%s" ("Sphere: " + (_str (vol_sphere (2.0))))
        printfn "%s" ("Hemisphere: " + (_str (vol_hemisphere (2.0))))
        printfn "%s" ("Circular Cylinder: " + (_str (vol_circular_cylinder (2.0) (2.0))))
        printfn "%s" ("Torus: " + (_str (vol_torus (2.0) (2.0))))
        printfn "%s" ("Conical Frustum: " + (_str (vol_conical_frustum (2.0) (2.0) (4.0))))
        printfn "%s" ("Spherical cap: " + (_str (vol_spherical_cap (1.0) (2.0))))
        printfn "%s" ("Spheres intersection: " + (_str (vol_spheres_intersect (2.0) (2.0) (1.0))))
        printfn "%s" ("Spheres union: " + (_str (vol_spheres_union (2.0) (2.0) (1.0))))
        printfn "%s" ("Hollow Circular Cylinder: " + (_str (vol_hollow_circular_cylinder (1.0) (2.0) (3.0))))
        printfn "%s" ("Icosahedron: " + (_str (vol_icosahedron (2.5))))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
