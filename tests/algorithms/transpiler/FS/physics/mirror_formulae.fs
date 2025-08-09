// Generated 2025-08-09 16:21 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
and abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and isclose (a: float) (b: float) (tolerance: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable tolerance = tolerance
    try
        __ret <- (abs_float (a - b)) < tolerance
        raise Return
        __ret
    with
        | Return -> __ret
and focal_length (distance_of_object: float) (distance_of_image: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable distance_of_object = distance_of_object
    let mutable distance_of_image = distance_of_image
    try
        if (distance_of_object = 0.0) || (distance_of_image = 0.0) then
            failwith ("Invalid inputs. Enter non zero values with respect to the sign convention.")
        __ret <- 1.0 / ((1.0 / distance_of_object) + (1.0 / distance_of_image))
        raise Return
        __ret
    with
        | Return -> __ret
and object_distance (focal_length: float) (distance_of_image: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable focal_length = focal_length
    let mutable distance_of_image = distance_of_image
    try
        if (distance_of_image = 0.0) || (focal_length = 0.0) then
            failwith ("Invalid inputs. Enter non zero values with respect to the sign convention.")
        __ret <- 1.0 / ((1.0 / focal_length) - (1.0 / distance_of_image))
        raise Return
        __ret
    with
        | Return -> __ret
and image_distance (focal_length: float) (distance_of_object: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable focal_length = focal_length
    let mutable distance_of_object = distance_of_object
    try
        if (distance_of_object = 0.0) || (focal_length = 0.0) then
            failwith ("Invalid inputs. Enter non zero values with respect to the sign convention.")
        __ret <- 1.0 / ((1.0 / focal_length) - (1.0 / distance_of_object))
        raise Return
        __ret
    with
        | Return -> __ret
and test_focal_length () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let f1: float = focal_length (10.0) (20.0)
        if not (isclose (f1) (6.66666666666666) (0.00000001)) then
            failwith ("focal_length test1 failed")
        let f2: float = focal_length (9.5) (6.7)
        if not (isclose (f2) (3.929012346) (0.00000001)) then
            failwith ("focal_length test2 failed")
        __ret
    with
        | Return -> __ret
and test_object_distance () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let u1: float = object_distance (30.0) (20.0)
        if not (isclose (u1) (-60.0) (0.00000001)) then
            failwith ("object_distance test1 failed")
        let u2: float = object_distance (10.5) (11.7)
        if not (isclose (u2) (102.375) (0.00000001)) then
            failwith ("object_distance test2 failed")
        __ret
    with
        | Return -> __ret
and test_image_distance () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let v1: float = image_distance (10.0) (40.0)
        if not (isclose (v1) (13.33333333) (0.00000001)) then
            failwith ("image_distance test1 failed")
        let v2: float = image_distance (1.5) (6.7)
        if not (isclose (v2) (1.932692308) (0.00000001)) then
            failwith ("image_distance test2 failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_focal_length()
        test_object_distance()
        test_image_distance()
        printfn "%s" (_str (focal_length (10.0) (20.0)))
        printfn "%s" (_str (object_distance (30.0) (20.0)))
        printfn "%s" (_str (image_distance (10.0) (40.0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
