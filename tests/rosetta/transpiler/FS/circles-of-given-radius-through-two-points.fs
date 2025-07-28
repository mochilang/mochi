// Generated 2025-07-28 01:10 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable g: float = x
        let mutable i: int = 0
        while i < 40 do
            g <- (g + (x / g)) / 2.0
            i <- i + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
let rec hypot (x: float) (y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    try
        __ret <- sqrtApprox ((x * x) + (y * y))
        raise Return
        __ret
    with
        | Return -> __ret
let Two: string = "Two circles."
let R0: string = "R==0.0 does not describe circles."
let Co: string = "Coincident points describe an infinite number of circles."
let CoR0: string = "Coincident points with r==0.0 describe a degenerate circle."
let Diam: string = "Points form a diameter and describe only a single circle."
let Far: string = "Points too far apart to form circles."
let rec circles (p1: Point) (p2: Point) (r: float) =
    let mutable __ret : obj array = Unchecked.defaultof<obj array>
    let mutable p1 = p1
    let mutable p2 = p2
    let mutable r = r
    try
        if ((p1.x) = (p2.x)) && ((p1.y) = (p2.y)) then
            if r = 0.0 then
                __ret <- [|box p1; box p1; box "Coincident points with r==0.0 describe a degenerate circle."|]
                raise Return
            __ret <- [|box p1; box p2; box "Coincident points describe an infinite number of circles."|]
            raise Return
        if r = 0.0 then
            __ret <- [|box p1; box p2; box "R==0.0 does not describe circles."|]
            raise Return
        let dx: float = (p2.x) - (p1.x)
        let dy: float = (p2.y) - (p1.y)
        let q: float = hypot dx dy
        if q > (2.0 * r) then
            __ret <- [|box p1; box p2; box "Points too far apart to form circles."|]
            raise Return
        let m: Point = { x = ((p1.x) + (p2.x)) / 2.0; y = ((p1.y) + (p2.y)) / 2.0 }
        if q = (2.0 * r) then
            __ret <- [|box m; box m; box "Points form a diameter and describe only a single circle."|]
            raise Return
        let d: float = sqrtApprox ((r * r) - ((q * q) / 4.0))
        let ox: float = (d * dx) / q
        let oy: float = (d * dy) / q
        __ret <- [|box { x = (m.x) - oy; y = (m.y) + ox }; box { x = (m.x) + oy; y = (m.y) - ox }; box "Two circles."|]
        raise Return
        __ret
    with
        | Return -> __ret
let mutable td = [|[|box { x = 0.1234; y = 0.9876 }; box { x = 0.8765; y = 0.2345 }; box 2.0|]; [|box { x = 0.0; y = 2.0 }; box { x = 0.0; y = 0.0 }; box 1.0|]; [|box { x = 0.1234; y = 0.9876 }; box { x = 0.1234; y = 0.9876 }; box 2.0|]; [|box { x = 0.1234; y = 0.9876 }; box { x = 0.8765; y = 0.2345 }; box 0.5|]; [|box { x = 0.1234; y = 0.9876 }; box { x = 0.1234; y = 0.9876 }; box 0.0|]|]
for tc in td do
    let p1: obj = box (tc.[0])
    let p2: obj = box (tc.[1])
    let r: obj = box (tc.[2])
    printfn "%s" (((("p1:  {" + (string (((p1 :?> Point).x)))) + " ") + (string (((p1 :?> Point).y)))) + "}")
    printfn "%s" (((("p2:  {" + (string (((p2 :?> Point).x)))) + " ") + (string (((p2 :?> Point).y)))) + "}")
    printfn "%s" ("r:  " + (string r))
    let res: obj array = circles (unbox<Point> p1) (unbox<Point> p2) (unbox<float> r)
    let c1: obj = res.[0]
    let c2: obj = res.[1]
    let caseStr: obj = res.[2]
    printfn "%s" ("   " + (unbox<string> caseStr))
    if ((unbox<string> caseStr) = "Points form a diameter and describe only a single circle.") || ((unbox<string> caseStr) = "Coincident points with r==0.0 describe a degenerate circle.") then
        printfn "%s" (((("   Center:  {" + (string (((c1 :?> Point).x)))) + " ") + (string (((c1 :?> Point).y)))) + "}")
    else
        if (unbox<string> caseStr) = "Two circles." then
            printfn "%s" (((("   Center 1:  {" + (string (((c1 :?> Point).x)))) + " ") + (string (((c1 :?> Point).y)))) + "}")
            printfn "%s" (((("   Center 2:  {" + (string (((c2 :?> Point).x)))) + " ") + (string (((c2 :?> Point).y)))) + "}")
    printfn "%s" ""
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
