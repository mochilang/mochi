// Generated 2025-07-26 05:05 +0700

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
type Point = {
    x: float
    y: float
}
type QuadSpline = {
    c0: float
    c1: float
    c2: float
}
type QuadCurve = {
    x: QuadSpline
    y: QuadSpline
}
type Anon1 = {
    p: obj
    q: obj
}
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
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
and minf (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a < b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and max3 (a: float) (b: float) (c: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let mutable m: float = a
        if b > m then
            m <- b
        if c > m then
            m <- c
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and min3 (a: float) (b: float) (c: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let mutable m: float = a
        if b < m then
            m <- b
        if c < m then
            m <- c
        __ret <- m
        raise Return
        __ret
    with
        | Return -> __ret
and subdivideQuadSpline (q: QuadSpline) (t: float) =
    let mutable __ret : QuadSpline array = Unchecked.defaultof<QuadSpline array>
    let mutable q = q
    let mutable t = t
    try
        let s: float = 1.0 - t
        let mutable u: QuadSpline = { c0 = q.c0; c1 = 0.0; c2 = 0.0 }
        let mutable v: QuadSpline = { c0 = 0.0; c1 = 0.0; c2 = q.c2 }
        u <- { u with c1 = (s * (q.c0)) + (t * (q.c1)) }
        v <- { v with c1 = (s * (q.c1)) + (t * (q.c2)) }
        u <- { u with c2 = (s * (u.c1)) + (t * (v.c1)) }
        v <- { v with c0 = u.c2 }
        __ret <- [|u; v|]
        raise Return
        __ret
    with
        | Return -> __ret
and subdivideQuadCurve (q: QuadCurve) (t: float) =
    let mutable __ret : QuadCurve array = Unchecked.defaultof<QuadCurve array>
    let mutable q = q
    let mutable t = t
    try
        let xs: QuadSpline array = subdivideQuadSpline (q.x) t
        let ys: QuadSpline array = subdivideQuadSpline (q.y) t
        let mutable u: QuadCurve = { x = xs.[0]; y = ys.[0] }
        let mutable v: QuadCurve = { x = xs.[1]; y = ys.[1] }
        __ret <- [|u; v|]
        raise Return
        __ret
    with
        | Return -> __ret
and rectsOverlap (xa0: float) (ya0: float) (xa1: float) (ya1: float) (xb0: float) (yb0: float) (xb1: float) (yb1: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xa0 = xa0
    let mutable ya0 = ya0
    let mutable xa1 = xa1
    let mutable ya1 = ya1
    let mutable xb0 = xb0
    let mutable yb0 = yb0
    let mutable xb1 = xb1
    let mutable yb1 = yb1
    try
        __ret <- (((xb0 <= xa1) && (xa0 <= xb1)) && (yb0 <= ya1)) && (ya0 <= yb1)
        raise Return
        __ret
    with
        | Return -> __ret
and testIntersect (p: QuadCurve) (q: QuadCurve) (tol: float) =
    let mutable __ret : Map<string, obj> = Unchecked.defaultof<Map<string, obj>>
    let mutable p = p
    let mutable q = q
    let mutable tol = tol
    try
        let pxmin: float = min3 (p.x.c0) (p.x.c1) (p.x.c2)
        let pymin: float = min3 (p.y.c0) (p.y.c1) (p.y.c2)
        let pxmax: float = max3 (p.x.c0) (p.x.c1) (p.x.c2)
        let pymax: float = max3 (p.y.c0) (p.y.c1) (p.y.c2)
        let qxmin: float = min3 (q.x.c0) (q.x.c1) (q.x.c2)
        let qymin: float = min3 (q.y.c0) (q.y.c1) (q.y.c2)
        let qxmax: float = max3 (q.x.c0) (q.x.c1) (q.x.c2)
        let qymax: float = max3 (q.y.c0) (q.y.c1) (q.y.c2)
        let mutable exclude: bool = true
        let mutable accept: bool = false
        let mutable inter: Point = { x = 0.0; y = 0.0 }
        if unbox<bool> (rectsOverlap pxmin pymin pxmax pymax qxmin qymin qxmax qymax) then
            exclude <- false
            let xmin: float = maxf pxmin qxmin
            let xmax: float = minf pxmax qxmax
            if (xmax - xmin) <= tol then
                let ymin: float = maxf pymin qymin
                let ymax: float = minf pymax qymax
                if (ymax - ymin) <= tol then
                    accept <- true
                    inter <- { inter with x = 0.5 * (xmin + xmax) }
                    inter <- { inter with y = 0.5 * (ymin + ymax) }
        __ret <- Map.ofList [("exclude", box exclude); ("accept", box accept); ("intersect", box inter)]
        raise Return
        __ret
    with
        | Return -> __ret
and seemsToBeDuplicate (pts: Point array) (xy: Point) (spacing: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable pts = pts
    let mutable xy = xy
    let mutable spacing = spacing
    try
        let mutable i: int = 0
        while i < (int (Array.length pts)) do
            let pt: Point = pts.[i]
            if ((float (absf ((pt.x) - (xy.x)))) < spacing) && ((float (absf ((pt.y) - (xy.y)))) < spacing) then
                __ret <- true
                raise Return
            i <- i + 1
        __ret <- false
        raise Return
        __ret
    with
        | Return -> __ret
and findIntersects (p: QuadCurve) (q: QuadCurve) (tol: float) (spacing: float) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable p = p
    let mutable q = q
    let mutable tol = tol
    let mutable spacing = spacing
    try
        let mutable inters: Point array = [||]
        let mutable workload: Map<string, QuadCurve> array = [|{ p = p; q = q }|]
        while (int (Array.length workload)) > 0 do
            let idx: int = (int (Array.length workload)) - 1
            let work: Map<string, QuadCurve> = workload.[idx]
            workload <- Array.sub workload 0 (idx - 0)
            let res: Map<string, obj> = testIntersect (unbox<QuadCurve> (work.["p"] |> unbox<QuadCurve>)) (unbox<QuadCurve> (work.["q"] |> unbox<QuadCurve>)) tol
            let excl: obj = box (res.["exclude"])
            let acc: obj = box (res.["accept"])
            let inter: Point = unbox<Point> (res.["intersect"])
            if unbox<bool> acc then
                if not (seemsToBeDuplicate inters inter spacing) then
                    inters <- Array.append inters [|inter|]
            else
                if not excl then
                    let ps: QuadCurve array = subdivideQuadCurve (unbox<QuadCurve> (work.["p"] |> unbox<QuadCurve>)) 0.5
                    let qs: QuadCurve array = subdivideQuadCurve (unbox<QuadCurve> (work.["q"] |> unbox<QuadCurve>)) 0.5
                    let p0: QuadCurve = ps.[0]
                    let p1: QuadCurve = ps.[1]
                    let q0: QuadCurve = qs.[0]
                    let q1: QuadCurve = qs.[1]
                    workload <- Array.append workload [|Map.ofList [("p", p0); ("q", q0)]|]
                    workload <- Array.append workload [|Map.ofList [("p", p0); ("q", q1)]|]
                    workload <- Array.append workload [|Map.ofList [("p", p1); ("q", q0)]|]
                    workload <- Array.append workload [|Map.ofList [("p", p1); ("q", q1)]|]
        __ret <- inters
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let p: QuadCurve = { x = { c0 = -1.0; c1 = 0.0; c2 = 1.0 }; y = { c0 = 0.0; c1 = 10.0; c2 = 0.0 } }
        let q: QuadCurve = { x = { c0 = 2.0; c1 = -8.0; c2 = 2.0 }; y = { c0 = 1.0; c1 = 2.0; c2 = 3.0 } }
        let tol: float = 0.0000001
        let spacing: float = tol * 10.0
        let inters: Point array = findIntersects p q tol spacing
        let mutable i: int = 0
        while i < (int (Array.length inters)) do
            let pt: Point = inters.[i]
            printfn "%s" (((("(" + (string (pt.x))) + ", ") + (string (pt.y))) + ")")
            i <- i + 1
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
