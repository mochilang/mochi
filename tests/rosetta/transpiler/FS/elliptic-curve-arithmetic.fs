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
type Pt = {
    x: float
    y: float
    inf: bool
}
let bCoeff: float = 7.0
let rec zero () =
    let mutable __ret : Pt = Unchecked.defaultof<Pt>
    try
        __ret <- { x = 0.0; y = 0.0; inf = true }
        raise Return
        __ret
    with
        | Return -> __ret
and isZero (p: Pt) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable p = p
    try
        __ret <- p.inf
        raise Return
        __ret
    with
        | Return -> __ret
and neg (p: Pt) =
    let mutable __ret : Pt = Unchecked.defaultof<Pt>
    let mutable p = p
    try
        __ret <- { x = p.x; y = -(p.y); inf = p.inf }
        raise Return
        __ret
    with
        | Return -> __ret
and dbl (p: Pt) =
    let mutable __ret : Pt = Unchecked.defaultof<Pt>
    let mutable p = p
    try
        if isZero p then
            __ret <- p
            raise Return
        let L: float = ((3.0 * (p.x)) * (p.x)) / (2.0 * (p.y))
        let x: float = (L * L) - (2.0 * (p.x))
        __ret <- { x = x; y = (L * ((p.x) - x)) - (p.y); inf = false }
        raise Return
        __ret
    with
        | Return -> __ret
and add (p: Pt) (q: Pt) =
    let mutable __ret : Pt = Unchecked.defaultof<Pt>
    let mutable p = p
    let mutable q = q
    try
        if isZero p then
            __ret <- q
            raise Return
        if isZero q then
            __ret <- p
            raise Return
        if (p.x) = (q.x) then
            if (p.y) = (q.y) then
                __ret <- dbl p
                raise Return
            __ret <- zero()
            raise Return
        let L: float = ((q.y) - (p.y)) / ((q.x) - (p.x))
        let x: float = ((L * L) - (p.x)) - (q.x)
        __ret <- { x = x; y = (L * ((p.x) - x)) - (p.y); inf = false }
        raise Return
        __ret
    with
        | Return -> __ret
and mul (p: Pt) (n: int) =
    let mutable __ret : Pt = Unchecked.defaultof<Pt>
    let mutable p = p
    let mutable n = n
    try
        let mutable r: Pt = zero()
        let mutable q: Pt = p
        let mutable k: int = n
        while k > 0 do
            if (((k % 2 + 2) % 2)) = 1 then
                r <- add r q
            q <- dbl q
            k <- k / 2
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and cbrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 40 do
            guess <- ((2.0 * guess) + (x / (guess * guess))) / 3.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and fromY (y: float) =
    let mutable __ret : Pt = Unchecked.defaultof<Pt>
    let mutable y = y
    try
        __ret <- { x = cbrtApprox ((y * y) - bCoeff); y = y; inf = false }
        raise Return
        __ret
    with
        | Return -> __ret
and show (s: string) (p: Pt) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable s = s
    let mutable p = p
    try
        if isZero p then
            printfn "%s" (s + "Zero")
        else
            printfn "%s" (((((s + "(") + (string (p.x))) + ", ") + (string (p.y))) + ")")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let a: Pt = fromY 1.0
        let b: Pt = fromY 2.0
        show "a = " a
        show "b = " b
        let c: Pt = add a b
        show "c = a + b = " c
        let d: Pt = neg c
        show "d = -c = " d
        show "c + d = " (add c d)
        show "a + b + d = " (add a (add b d))
        show "a * 12345 = " (mul a 12345)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
