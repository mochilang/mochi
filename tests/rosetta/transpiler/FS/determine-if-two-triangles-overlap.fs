// Generated 2025-07-31 00:10 +0700

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

type Point = {
    x: float
    y: float
}
type Triangle = {
    p1: Point
    p2: Point
    p3: Point
}
let rec fmt1 (f: float) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    try
        let mutable s: string = string f
        let idx: int = s.IndexOf(".")
        if idx < 0 then
            s <- s + ".0"
        else
            let mutable need: int = idx + 2
            if (String.length s) > need then
                s <- _substring s 0 need
            else
                while (String.length s) < need do
                    s <- s + "0"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
and pointStr (p: Point) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        __ret <- ((("(" + (unbox<string> (fmt1 (p.x)))) + ", ") + (unbox<string> (fmt1 (p.y)))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
and triangleStr (t: Triangle) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable t = t
    try
        __ret <- (((("Triangle " + (unbox<string> (pointStr (t.p1)))) + ", ") + (unbox<string> (pointStr (t.p2)))) + ", ") + (unbox<string> (pointStr (t.p3)))
        raise Return
        __ret
    with
        | Return -> __ret
and orient (a: Point) (b: Point) (c: Point) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        __ret <- (((b.x) - (a.x)) * ((c.y) - (a.y))) - (((b.y) - (a.y)) * ((c.x) - (a.x)))
        raise Return
        __ret
    with
        | Return -> __ret
and pointInTri (p: Point) (t: Triangle) (onBoundary: bool) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p = p
    let mutable t = t
    let mutable onBoundary = onBoundary
    try
        let d1: float = orient p (t.p1) (t.p2)
        let d2: float = orient p (t.p2) (t.p3)
        let d3: float = orient p (t.p3) (t.p1)
        let mutable hasNeg: bool = ((d1 < 0.0) || (d2 < 0.0)) || (d3 < 0.0)
        let mutable hasPos: bool = ((d1 > 0.0) || (d2 > 0.0)) || (d3 > 0.0)
        if onBoundary then
            __ret <- not (hasNeg && hasPos)
            raise Return
        __ret <- (((not (hasNeg && hasPos)) && (d1 <> 0.0)) && (d2 <> 0.0)) && (d3 <> 0.0)
        raise Return
        __ret
    with
        | Return -> __ret
and edgeCheck (a0: Point) (a1: Point) (bs: Point array) (onBoundary: bool) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a0 = a0
    let mutable a1 = a1
    let mutable bs = bs
    let mutable onBoundary = onBoundary
    try
        let d0: float = orient a0 a1 (bs.[0])
        let d1: float = orient a0 a1 (bs.[1])
        let d2: float = orient a0 a1 (bs.[2])
        if onBoundary then
            __ret <- ((d0 <= 0.0) && (d1 <= 0.0)) && (d2 <= 0.0)
            raise Return
        __ret <- ((d0 < 0.0) && (d1 < 0.0)) && (d2 < 0.0)
        raise Return
        __ret
    with
        | Return -> __ret
and triTri2D (t1: Triangle) (t2: Triangle) (onBoundary: bool) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable t1 = t1
    let mutable t2 = t2
    let mutable onBoundary = onBoundary
    try
        let a: Point array = [|t1.p1; t1.p2; t1.p3|]
        let b: Point array = [|t2.p1; t2.p2; t2.p3|]
        let mutable i: int = 0
        while i < 3 do
            let j: int = (((i + 1) % 3 + 3) % 3)
            if edgeCheck (a.[i]) (a.[j]) b onBoundary then
                __ret <- false
                raise Return
            i <- i + 1
        i <- 0
        while i < 3 do
            let j: int = (((i + 1) % 3 + 3) % 3)
            if edgeCheck (b.[i]) (b.[j]) a onBoundary then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
and iff (cond: bool) (a: string) (b: string) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable cond = cond
    let mutable a = a
    let mutable b = b
    try
        if cond then
            __ret <- a
            raise Return
        else
            __ret <- b
            raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable t1: Triangle = { p1 = { x = 0.0; y = 0.0 }; p2 = { x = 5.0; y = 0.0 }; p3 = { x = 0.0; y = 5.0 } }
        let mutable t2: Triangle = { p1 = { x = 0.0; y = 0.0 }; p2 = { x = 5.0; y = 0.0 }; p3 = { x = 0.0; y = 6.0 } }
        printfn "%s" ((unbox<string> (triangleStr t1)) + " and")
        printfn "%s" (triangleStr t2)
        let mutable overlapping: bool = triTri2D t1 t2 true
        printfn "%s" (iff overlapping "overlap" "do not overlap")
        printfn "%s" ""
        t1 <- { p1 = { x = 0.0; y = 0.0 }; p2 = { x = 0.0; y = 5.0 }; p3 = { x = 5.0; y = 0.0 } }
        t2 <- t1
        printfn "%s" ((unbox<string> (triangleStr t1)) + " and")
        printfn "%s" (triangleStr t2)
        overlapping <- triTri2D t1 t2 true
        printfn "%s" (iff overlapping "overlap (reversed)" "do not overlap")
        printfn "%s" ""
        t1 <- { p1 = { x = 0.0; y = 0.0 }; p2 = { x = 5.0; y = 0.0 }; p3 = { x = 0.0; y = 5.0 } }
        t2 <- { p1 = { x = -10.0; y = 0.0 }; p2 = { x = -5.0; y = 0.0 }; p3 = { x = -1.0; y = 6.0 } }
        printfn "%s" ((unbox<string> (triangleStr t1)) + " and")
        printfn "%s" (triangleStr t2)
        overlapping <- triTri2D t1 t2 true
        printfn "%s" (iff overlapping "overlap" "do not overlap")
        printfn "%s" ""
        t1 <- { t1 with p3 = { x = 2.5; y = 5.0 } }
        t2 <- { p1 = { x = 0.0; y = 4.0 }; p2 = { x = 2.5; y = -1.0 }; p3 = { x = 5.0; y = 4.0 } }
        printfn "%s" ((unbox<string> (triangleStr t1)) + " and")
        printfn "%s" (triangleStr t2)
        overlapping <- triTri2D t1 t2 true
        printfn "%s" (iff overlapping "overlap" "do not overlap")
        printfn "%s" ""
        t1 <- { p1 = { x = 0.0; y = 0.0 }; p2 = { x = 1.0; y = 1.0 }; p3 = { x = 0.0; y = 2.0 } }
        t2 <- { p1 = { x = 2.0; y = 1.0 }; p2 = { x = 3.0; y = 0.0 }; p3 = { x = 3.0; y = 2.0 } }
        printfn "%s" ((unbox<string> (triangleStr t1)) + " and")
        printfn "%s" (triangleStr t2)
        overlapping <- triTri2D t1 t2 true
        printfn "%s" (iff overlapping "overlap" "do not overlap")
        printfn "%s" ""
        t2 <- { p1 = { x = 2.0; y = 1.0 }; p2 = { x = 3.0; y = -2.0 }; p3 = { x = 3.0; y = 4.0 } }
        printfn "%s" ((unbox<string> (triangleStr t1)) + " and")
        printfn "%s" (triangleStr t2)
        overlapping <- triTri2D t1 t2 true
        printfn "%s" (iff overlapping "overlap" "do not overlap")
        printfn "%s" ""
        t1 <- { p1 = { x = 0.0; y = 0.0 }; p2 = { x = 1.0; y = 0.0 }; p3 = { x = 0.0; y = 1.0 } }
        t2 <- { p1 = { x = 1.0; y = 0.0 }; p2 = { x = 2.0; y = 0.0 }; p3 = { x = 1.0; y = 1.1 } }
        printfn "%s" ((unbox<string> (triangleStr t1)) + " and")
        printfn "%s" (triangleStr t2)
        printfn "%s" "which have only a single corner in contact, if boundary points collide"
        overlapping <- triTri2D t1 t2 true
        printfn "%s" (iff overlapping "overlap" "do not overlap")
        printfn "%s" ""
        printfn "%s" ((unbox<string> (triangleStr t1)) + " and")
        printfn "%s" (triangleStr t2)
        printfn "%s" "which have only a single corner in contact, if boundary points do not collide"
        overlapping <- triTri2D t1 t2 false
        printfn "%s" (iff overlapping "overlap" "do not overlap")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
