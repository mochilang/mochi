// Generated 2025-07-30 21:41 +0700

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
type Rsdv = {
    n: float
    a: float
    q: float
}
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable g: float = x
        let mutable i: int = 0
        while i < 20 do
            g <- (g + (x / g)) / 2.0
            i <- i + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and newRsdv () =
    let mutable __ret : Rsdv = Unchecked.defaultof<Rsdv>
    try
        __ret <- { n = 0.0; a = 0.0; q = 0.0 }
        raise Return
        __ret
    with
        | Return -> __ret
and add (r: Rsdv) (x: float) =
    let mutable __ret : Rsdv = Unchecked.defaultof<Rsdv>
    let mutable r = r
    let mutable x = x
    try
        let n1: float = (r.n) + 1.0
        let a1: float = (r.a) + ((x - (r.a)) / n1)
        let q1: float = (r.q) + ((x - (r.a)) * (x - a1))
        __ret <- { n = n1; a = a1; q = q1 }
        raise Return
        __ret
    with
        | Return -> __ret
and sd (r: Rsdv) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    try
        __ret <- sqrtApprox ((r.q) / (r.n))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let mutable r: Rsdv = newRsdv()
        for x in [|2.0; 4.0; 4.0; 4.0; 5.0; 5.0; 7.0; 9.0|] do
            r <- add r (float x)
            printfn "%s" (string (sd r))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
