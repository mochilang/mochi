// Generated 2025-07-28 07:48 +0700

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
    x: int
    y: int
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec ccw (a: Point) (b: Point) (c: Point) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable c = c
    try
        let lhs: int = ((b.x) - (a.x)) * ((c.y) - (a.y))
        let rhs: int = ((b.y) - (a.y)) * ((c.x) - (a.x))
        __ret <- lhs > rhs
        raise Return
        __ret
    with
        | Return -> __ret
let rec sortPoints (ps: Point array) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable ps = ps
    try
        let mutable arr: Point array = ps
        let mutable n: int = Seq.length arr
        let mutable i: int = 0
        while i < n do
            let mutable j: int = 0
            while j < (n - 1) do
                let p: Point = arr.[j]
                let q: Point = arr.[j + 1]
                if ((p.x) > (q.x)) || (((p.x) = (q.x)) && ((p.y) > (q.y))) then
                    arr.[j] <- q
                    arr.[j + 1] <- p
                j <- j + 1
            i <- i + 1
        __ret <- arr
        raise Return
        __ret
    with
        | Return -> __ret
let rec convexHull (ps: Point array) =
    let mutable __ret : Point array = Unchecked.defaultof<Point array>
    let mutable ps = ps
    try
        ps <- sortPoints ps
        let mutable h: Point array = [||]
        for pt in ps do
            while ((Seq.length h) >= 2) && ((unbox<bool> (ccw (h.[(Seq.length h) - 2]) (h.[(Seq.length h) - 1]) pt)) = false) do
                h <- Array.sub h 0 (((Seq.length h) - 1) - 0)
            h <- Array.append h [|pt|]
        let mutable i: int = (Seq.length ps) - 2
        let t: int = (Seq.length h) + 1
        while i >= 0 do
            let pt: Point = ps.[i]
            while ((Seq.length h) >= t) && ((unbox<bool> (ccw (h.[(Seq.length h) - 2]) (h.[(Seq.length h) - 1]) pt)) = false) do
                h <- Array.sub h 0 (((Seq.length h) - 1) - 0)
            h <- Array.append h [|pt|]
            i <- i - 1
        __ret <- Array.sub h 0 (((Seq.length h) - 1) - 0)
        raise Return
        __ret
    with
        | Return -> __ret
let rec pointStr (p: Point) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable p = p
    try
        __ret <- ((("(" + (string (p.x))) + ",") + (string (p.y))) + ")"
        raise Return
        __ret
    with
        | Return -> __ret
let rec hullStr (h: Point array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable h = h
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length h) do
            s <- s + (unbox<string> (pointStr (h.[i])))
            if i < ((Seq.length h) - 1) then
                s <- s + " "
            i <- i + 1
        s <- s + "]"
        __ret <- s
        raise Return
        __ret
    with
        | Return -> __ret
let pts: Point array = [|{ x = 16; y = 3 }; { x = 12; y = 17 }; { x = 0; y = 6 }; { x = -4; y = -6 }; { x = 16; y = 6 }; { x = 16; y = -7 }; { x = 16; y = -3 }; { x = 17; y = -4 }; { x = 5; y = 19 }; { x = 19; y = -8 }; { x = 3; y = 16 }; { x = 12; y = 13 }; { x = 3; y = -4 }; { x = 17; y = 5 }; { x = -3; y = 15 }; { x = -3; y = -9 }; { x = 0; y = 11 }; { x = -9; y = -3 }; { x = -4; y = -2 }; { x = 12; y = 10 }|]
let hull: Point array = convexHull pts
printfn "%s" ("Convex Hull: " + (unbox<string> (hullStr hull)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
