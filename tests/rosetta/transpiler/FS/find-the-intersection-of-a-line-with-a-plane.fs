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
type Vector = {
    x: float
    y: float
    z: float
}
let rec add (a: Vector) (b: Vector) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { x = (a.x) + (b.x); y = (a.y) + (b.y); z = (a.z) + (b.z) }
        raise Return
        __ret
    with
        | Return -> __ret
and sub (a: Vector) (b: Vector) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable a = a
    let mutable b = b
    try
        __ret <- { x = (a.x) - (b.x); y = (a.y) - (b.y); z = (a.z) - (b.z) }
        raise Return
        __ret
    with
        | Return -> __ret
and mul (v: Vector) (s: float) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable v = v
    let mutable s = s
    try
        __ret <- { x = (v.x) * s; y = (v.y) * s; z = (v.z) * s }
        raise Return
        __ret
    with
        | Return -> __ret
and dot (a: Vector) (b: Vector) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- (((a.x) * (b.x)) + ((a.y) * (b.y))) + ((a.z) * (b.z))
        raise Return
        __ret
    with
        | Return -> __ret
and intersectPoint (rv: Vector) (rp: Vector) (pn: Vector) (pp: Vector) =
    let mutable __ret : Vector = Unchecked.defaultof<Vector>
    let mutable rv = rv
    let mutable rp = rp
    let mutable pn = pn
    let mutable pp = pp
    try
        let diff: Vector = sub rp pp
        let prod1: float = dot diff pn
        let prod2: float = dot rv pn
        let prod3: float = prod1 / prod2
        __ret <- sub rp (mul rv prod3)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let rv: Vector = { x = 0.0; y = -1.0; z = -1.0 }
        let rp: Vector = { x = 0.0; y = 0.0; z = 10.0 }
        let pn: Vector = { x = 0.0; y = 0.0; z = 1.0 }
        let pp: Vector = { x = 0.0; y = 0.0; z = 5.0 }
        let ip: Vector = intersectPoint rv rp pn pp
        printfn "%s" (((((("The ray intersects the plane at (" + (string (ip.x))) + ", ") + (string (ip.y))) + ", ") + (string (ip.z))) + ")")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
