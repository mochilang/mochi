// Generated 2025-08-07 16:27 +0700

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
let rec floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
let rec modf (x: float) (m: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable m = m
    try
        __ret <- x - ((floor (x / m)) * m)
        raise Return
        __ret
    with
        | Return -> __ret
let rec sin_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable i: int = 1
        while i < 10 do
            let k1: float = 2.0 * (float i)
            let k2: float = k1 + 1.0
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec cos_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            let k1: float = (2.0 * (float i)) - 1.0
            let k2: float = 2.0 * (float i)
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec convert_to_2d (x: float) (y: float) (z: float) (scale: float) (distance: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable x = x
    let mutable y = y
    let mutable z = z
    let mutable scale = scale
    let mutable distance = distance
    try
        let projected_x: float = ((x * distance) / (z + distance)) * scale
        let projected_y: float = ((y * distance) / (z + distance)) * scale
        __ret <- unbox<float array> [|projected_x; projected_y|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec rotate (x: float) (y: float) (z: float) (axis: string) (angle: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable x = x
    let mutable y = y
    let mutable z = z
    let mutable axis = axis
    let mutable angle = angle
    try
        let mutable angle: float = (((modf (angle) (360.0)) / 450.0) * 180.0) / PI
        angle <- modf (angle) (2.0 * PI)
        if angle > PI then
            angle <- angle - (2.0 * PI)
        if axis = "z" then
            let new_x: float = (x * (cos_taylor (angle))) - (y * (sin_taylor (angle)))
            let new_y: float = (y * (cos_taylor (angle))) + (x * (sin_taylor (angle)))
            let new_z: float = z
            __ret <- unbox<float array> [|new_x; new_y; new_z|]
            raise Return
        if axis = "x" then
            let new_y: float = (y * (cos_taylor (angle))) - (z * (sin_taylor (angle)))
            let new_z: float = (z * (cos_taylor (angle))) + (y * (sin_taylor (angle)))
            let new_x: float = x
            __ret <- unbox<float array> [|new_x; new_y; new_z|]
            raise Return
        if axis = "y" then
            let new_x: float = (x * (cos_taylor (angle))) - (z * (sin_taylor (angle)))
            let new_z: float = (z * (cos_taylor (angle))) + (x * (sin_taylor (angle)))
            let new_y: float = y
            __ret <- unbox<float array> [|new_x; new_y; new_z|]
            raise Return
        printfn "%s" ("not a valid axis, choose one of 'x', 'y', 'z'")
        __ret <- unbox<float array> [|0.0; 0.0; 0.0|]
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (convert_to_2d (1.0) (2.0) (3.0) (10.0) (10.0)))
printfn "%s" (_str (rotate (1.0) (2.0) (3.0) ("y") (90.0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
