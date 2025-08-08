// Generated 2025-08-08 18:09 +0700

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
type Point3d = {
    mutable x: float
    mutable y: float
    mutable z: float
}
type Vector3d = {
    mutable x: float
    mutable y: float
    mutable z: float
}
let rec create_vector (p1: Point3d) (p2: Point3d) =
    let mutable __ret : Vector3d = Unchecked.defaultof<Vector3d>
    let mutable p1 = p1
    let mutable p2 = p2
    try
        let vx: float = (p2.x) - (p1.x)
        let vy: float = (p2.y) - (p1.y)
        let vz: float = (p2.z) - (p1.z)
        __ret <- { x = vx; y = vy; z = vz }
        raise Return
        __ret
    with
        | Return -> __ret
and get_3d_vectors_cross (ab: Vector3d) (ac: Vector3d) =
    let mutable __ret : Vector3d = Unchecked.defaultof<Vector3d>
    let mutable ab = ab
    let mutable ac = ac
    try
        let cx: float = ((ab.y) * (ac.z)) - ((ab.z) * (ac.y))
        let cy: float = ((ab.z) * (ac.x)) - ((ab.x) * (ac.z))
        let cz: float = ((ab.x) * (ac.y)) - ((ab.y) * (ac.x))
        __ret <- { x = cx; y = cy; z = cz }
        raise Return
        __ret
    with
        | Return -> __ret
and pow10 (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * 10.0
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and round_float (x: float) (digits: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable digits = digits
    try
        let factor: float = pow10 (digits)
        let mutable v: float = x * factor
        if v >= 0.0 then
            v <- v + 0.5
        else
            v <- v - 0.5
        let t: int = int v
        __ret <- (float t) / factor
        raise Return
        __ret
    with
        | Return -> __ret
and is_zero_vector (v: Vector3d) (accuracy: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable v = v
    let mutable accuracy = accuracy
    try
        __ret <- (((round_float (v.x) (accuracy)) = 0.0) && ((round_float (v.y) (accuracy)) = 0.0)) && ((round_float (v.z) (accuracy)) = 0.0)
        raise Return
        __ret
    with
        | Return -> __ret
and are_collinear (a: Point3d) (b: Point3d) (c: Point3d) (accuracy: int) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    let mutable accuracy = accuracy
    try
        let ab: Vector3d = create_vector (a) (b)
        let ac: Vector3d = create_vector (a) (c)
        let cross: Vector3d = get_3d_vectors_cross (ab) (ac)
        __ret <- is_zero_vector (cross) (accuracy)
        raise Return
        __ret
    with
        | Return -> __ret
and test_are_collinear () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let p1: Point3d = { x = 0.0; y = 0.0; z = 0.0 }
        let p2: Point3d = { x = 1.0; y = 1.0; z = 1.0 }
        let p3: Point3d = { x = 2.0; y = 2.0; z = 2.0 }
        if not (are_collinear (p1) (p2) (p3) (10)) then
            failwith ("collinear test failed")
        let q3: Point3d = { x = 1.0; y = 2.0; z = 3.0 }
        if are_collinear (p1) (p2) (q3) (10) then
            failwith ("non-collinear test failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_are_collinear()
        let a: Point3d = { x = 4.802293498137402; y = 3.536233125455244; z = 0.0 }
        let b: Point3d = { x = -2.186788107953106; y = -9.24561398001649; z = 7.141509524846482 }
        let c: Point3d = { x = 1.530169574640268; y = -2.447927606600034; z = 3.343487096469054 }
        printfn "%s" (_str (are_collinear (a) (b) (c) (10)))
        let d: Point3d = { x = 2.399001826862445; y = -2.452009976680793; z = 4.464656666157666 }
        let e: Point3d = { x = -3.682816335934376; y = 5.753788986533145; z = 9.490993909044244 }
        let f: Point3d = { x = 1.962903518985307; y = 3.741415730125627; z = 7.0 }
        printfn "%s" (_str (are_collinear (d) (e) (f) (10)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
