// Generated 2025-07-28 10:03 +0700

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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

type V3 = {
    x: float
    y: float
    z: float
}
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and powf (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and normalize (v: V3) =
    let mutable __ret : V3 = Unchecked.defaultof<V3>
    let mutable v = v
    try
        let len: float = sqrtApprox ((((v.x) * (v.x)) + ((v.y) * (v.y))) + ((v.z) * (v.z)))
        __ret <- { x = (v.x) / len; y = (v.y) / len; z = (v.z) / len }
        raise Return
        __ret
    with
        | Return -> __ret
and dot (a: V3) (b: V3) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        let d: float = (((a.x) * (b.x)) + ((a.y) * (b.y))) + ((a.z) * (b.z))
        if d < 0.0 then
            __ret <- -d
            raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
and drawSphere (r: int) (k: int) (ambient: float) (light: V3) (shades: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable r = r
    let mutable k = k
    let mutable ambient = ambient
    let mutable light = light
    let mutable shades = shades
    try
        let mutable i: int = -r
        while i <= r do
            let x: float = (float i) + 0.5
            let mutable line: string = ""
            let mutable j: int = -(2 * r)
            while j <= (2 * r) do
                let y: float = ((float j) / 2.0) + 0.5
                if ((x * x) + (y * y)) <= ((float r) * (float r)) then
                    let mutable zsq: float = (((float r) * (float r)) - (x * x)) - (y * y)
                    let mutable vec: V3 = normalize { x = x; y = y; z = sqrtApprox zsq }
                    let mutable b: float = (float (powf (dot light vec) k)) + ambient
                    let mutable intensity: int = int ((1.0 - b) * ((float (String.length shades)) - 1.0))
                    if intensity < 0 then
                        intensity <- 0
                    if intensity >= (String.length shades) then
                        intensity <- (String.length shades) - 1
                    line <- line + (_substring shades intensity (intensity + 1))
                else
                    line <- line + " "
                j <- j + 1
            printfn "%s" line
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let shades: string = ".:!*oe&#%@"
        let mutable light: V3 = normalize { x = 30.0; y = 30.0; z = -50.0 }
        drawSphere 20 4 0.1 light shades
        drawSphere 10 2 0.4 light shades
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
