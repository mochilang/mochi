// Generated 2025-07-31 00:10 +0700

exception Break
exception Continue

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
type Sphere = {
    cx: float
    cy: float
    cz: float
    r: float
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
and hitSphere (s: Sphere) (x: float) (y: float) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable s = s
    let mutable x = x
    let mutable y = y
    try
        let dx: float = x - (s.cx)
        let dy: float = y - (s.cy)
        let zsq: float = ((s.r) * (s.r)) - ((dx * dx) + (dy * dy))
        if zsq < 0.0 then
            __ret <- unbox<Map<string, obj>> (Map.ofList [("hit", box false)])
            raise Return
        let z: float = sqrtApprox zsq
        __ret <- unbox<Map<string, obj>> (Map.ofList [("hit", box true); ("z1", box ((s.cz) - z)); ("z2", box ((s.cz) + z))])
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let shades: string = ".:!*oe&#%@"
        let mutable light: V3 = normalize { x = -50.0; y = 30.0; z = 50.0 }
        let pos: Sphere = { cx = 20.0; cy = 20.0; cz = 0.0; r = 20.0 }
        let neg: Sphere = { cx = 1.0; cy = 1.0; cz = -6.0; r = 20.0 }
        let mutable yi: int = 0
        try
            while yi <= 40 do
                try
                    let y: float = (float yi) + 0.5
                    let mutable line: string = ""
                    let mutable xi: int = -20
                    try
                        while xi <= 60 do
                            try
                                let x: float = ((((float xi) - (pos.cx)) / 2.0) + 0.5) + (pos.cx)
                                let hb: Map<string, obj> = hitSphere pos x y
                                if not (hb.["hit"]) then
                                    line <- line + " "
                                    xi <- xi + 1
                                    raise Continue
                                let zb1 = hb.["z1"]
                                let zb2 = hb.["z2"]
                                let hs: Map<string, obj> = hitSphere neg x y
                                let mutable hitRes: int = 1
                                if not (hs.["hit"]) then
                                    hitRes <- 1
                                else
                                    if (hs.["z1"]) > zb1 then
                                        hitRes <- 1
                                    else
                                        if (hs.["z2"]) > zb2 then
                                            hitRes <- 0
                                        else
                                            if (hs.["z2"]) > zb1 then
                                                hitRes <- 2
                                            else
                                                hitRes <- 1
                                if hitRes = 0 then
                                    line <- line + " "
                                    xi <- xi + 1
                                    raise Continue
                                let mutable vec: V3 = 0
                                if hitRes = 1 then
                                    vec <- { x = x - (pos.cx); y = y - (pos.cy); z = (float zb1) - (pos.cz) }
                                else
                                    vec <- { x = (neg.cx) - x; y = (neg.cy) - y; z = (neg.cz) - (float (hs.["z2"])) }
                                vec <- normalize vec
                                let mutable b: float = (float (powf (dot light vec) 2)) + 0.5
                                let mutable intensity: int = int ((1.0 - b) * (float (String.length shades)))
                                if intensity < 0 then
                                    intensity <- 0
                                if intensity >= (String.length shades) then
                                    intensity <- (String.length shades) - 1
                                line <- line + (_substring shades intensity (intensity + 1))
                                xi <- xi + 1
                            with
                            | Continue -> ()
                            | Break -> raise Break
                    with
                    | Break -> ()
                    | Continue -> ()
                    printfn "%s" line
                    yi <- yi + 1
                with
                | Continue -> ()
                | Break -> raise Break
        with
        | Break -> ()
        | Continue -> ()
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
