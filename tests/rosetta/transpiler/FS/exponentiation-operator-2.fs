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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec expI (b: int) (p: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable b = b
    let mutable p = p
    try
        let mutable r: int = 1
        let mutable i: int = 0
        while i < p do
            r <- r * b
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and expF (b: float) (p: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable b = b
    let mutable p = p
    try
        let mutable r: float = 1.0
        let mutable pow: float = b
        let mutable n: int = p
        let mutable neg: bool = false
        if p < 0 then
            n <- -p
            neg <- true
        while n > 0 do
            if (((n % 2 + 2) % 2)) = 1 then
                r <- r * pow
            pow <- pow * pow
            n <- n / 2
        if neg then
            r <- 1.0 / r
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and printExpF (b: float) (p: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable b = b
    let mutable p = p
    try
        if (b = 0.0) && (p < 0) then
            printfn "%s" ((((string (b)) + "^") + (string (p))) + ": +Inf")
        else
            printfn "%s" (((((string (b)) + "^") + (string (p))) + ": ") + (string (expF (b) (p))))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" ("expI tests")
        for pair in [|[|2; 10|]; [|2; -10|]; [|-2; 10|]; [|-2; 11|]; [|11; 0|]|] do
            if (unbox<int> (_idx pair 1)) < 0 then
                printfn "%s" ((((string (_idx pair 0)) + "^") + (string (_idx pair 1))) + ": negative power not allowed")
            else
                printfn "%s" (((((string (_idx pair 0)) + "^") + (string (_idx pair 1))) + ": ") + (string (expI (unbox<int> (_idx pair 0)) (unbox<int> (_idx pair 1)))))
        printfn "%s" ("overflow undetected")
        printfn "%s" ("10^10: " + (string (expI (10) (10))))
        printfn "%s" ("\nexpF tests:")
        for pair in [|[|box (2.0); box (10)|]; [|box (2.0); box (-10)|]; [|box (-2.0); box (10)|]; [|box (-2.0); box (11)|]; [|box (11.0); box (0)|]|] do
            printExpF (System.Convert.ToDouble (_idx pair 0)) (unbox<int> (_idx pair 1))
        printfn "%s" ("disallowed in expI, allowed here")
        printExpF (0.0) (-1)
        printfn "%s" ("other interesting cases for 32 bit float type")
        printExpF (10.0) (39)
        printExpF (10.0) (-39)
        printExpF (-10.0) (39)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
