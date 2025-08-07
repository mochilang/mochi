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
type Point = {
    x: int
    y: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec get_mid (p1: Point) (p2: Point) =
    let mutable __ret : Point = Unchecked.defaultof<Point>
    let mutable p1 = p1
    let mutable p2 = p2
    try
        __ret <- { x = ((p1.x) + (p2.x)) / 2; y = ((p1.y) + (p2.y)) / 2 }
        raise Return
        __ret
    with
        | Return -> __ret
let rec point_to_string (p: Point) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        __ret <- ((("(" + (_str (p.x))) + ",") + (_str (p.y))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let rec triangle (v1: Point) (v2: Point) (v3: Point) (depth: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable v1 = v1
    let mutable v2 = v2
    let mutable v3 = v3
    let mutable depth = depth
    try
        printfn "%s" (((((point_to_string (v1)) + " ") + (point_to_string (v2))) + " ") + (point_to_string (v3)))
        if depth = 0 then
            __ret <- ()
            raise Return
        triangle (v1) (get_mid (v1) (v2)) (get_mid (v1) (v3)) (depth - 1)
        triangle (v2) (get_mid (v1) (v2)) (get_mid (v2) (v3)) (depth - 1)
        triangle (v3) (get_mid (v3) (v2)) (get_mid (v1) (v3)) (depth - 1)
        __ret
    with
        | Return -> __ret
triangle ({ x = -175; y = -125 }) ({ x = 0; y = 175 }) ({ x = 175; y = -125 }) (2)
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
