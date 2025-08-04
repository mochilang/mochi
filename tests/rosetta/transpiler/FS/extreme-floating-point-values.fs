// Generated 2025-08-04 20:03 +0700

exception Return
let mutable __ret = ()

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
let rec makeInf () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        let mutable x: float = 1.0
        let mutable i: int = 0
        while i < 400 do
            x <- x * 10.0
            i <- i + 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and makeMax () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        let mutable x: float = 1.0
        let mutable i: int = 0
        while i < 308 do
            x <- x * 10.0
            i <- i + 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
and isNaN (x: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable x = x
    try
        __ret <- x <> x
        raise Return
        __ret
    with
        | Return -> __ret
and validateNaN (n: float) (op: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable op = op
    try
        if isNaN (n) then
            printfn "%s" (op + " -> NaN")
        else
            printfn "%s" (String.concat (" ") ([|sprintf "%s" ("!!! Expected NaN from"); sprintf "%s" (op); sprintf "%s" (" Found"); sprintf "%g" (n)|]))
        __ret
    with
        | Return -> __ret
and validateZero (n: float) (op: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    let mutable op = op
    try
        if n = (float 0) then
            printfn "%s" (op + " -> 0")
        else
            printfn "%s" (String.concat (" ") ([|sprintf "%s" ("!!! Expected 0 from"); sprintf "%s" (op); sprintf "%s" (" Found"); sprintf "%g" (n)|]))
        __ret
    with
        | Return -> __ret
and validateGT (a: float) (b: float) (op: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable a = a
    let mutable b = b
    let mutable op = op
    try
        if a > b then
            printfn "%s" (op)
        else
            printfn "%s" (String.concat (" ") ([|sprintf "%s" ("!!! Expected"); sprintf "%s" (op); sprintf "%s" (" Found not true.")|]))
        __ret
    with
        | Return -> __ret
and validateNE (a: float) (b: float) (op: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable a = a
    let mutable b = b
    let mutable op = op
    try
        if a = b then
            printfn "%s" (String.concat (" ") ([|sprintf "%s" ("!!! Expected"); sprintf "%s" (op); sprintf "%s" (" Found not true.")|]))
        else
            printfn "%s" (op)
        __ret
    with
        | Return -> __ret
and validateEQ (a: float) (b: float) (op: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable a = a
    let mutable b = b
    let mutable op = op
    try
        if a = b then
            printfn "%s" (op)
        else
            printfn "%s" (String.concat (" ") ([|sprintf "%s" ("!!! Expected"); sprintf "%s" (op); sprintf "%s" (" Found not true.")|]))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let negZero: float = -0.0
        let posInf: float = makeInf()
        let negInf: float = -posInf
        let nan: float = posInf / posInf
        let maxVal: float = makeMax()
        printfn "%s" (String.concat (" ") ([|sprintf "%g" (negZero); sprintf "%g" (posInf); sprintf "%g" (negInf); sprintf "%g" (nan)|]))
        printfn "%s" (String.concat (" ") ([|sprintf "%g" (negZero); sprintf "%g" (posInf); sprintf "%g" (negInf); sprintf "%g" (nan)|]))
        printfn "%s" ("")
        validateNaN (negInf + posInf) ("-Inf + Inf")
        validateNaN (0.0 * posInf) ("0 * Inf")
        validateNaN (posInf / posInf) ("Inf / Inf")
        validateNaN (((posInf % 1.0 + 1.0) % 1.0)) ("Inf % 1")
        validateNaN (1.0 + nan) ("1 + NaN")
        validateZero (1.0 / posInf) ("1 / Inf")
        validateGT (posInf) (maxVal) ("Inf > max value")
        validateGT (-maxVal) (negInf) ("-Inf < max neg value")
        validateNE (nan) (nan) ("NaN != NaN")
        validateEQ (negZero) (0.0) ("-0 == 0")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
