// Generated 2025-08-04 20:44 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec printFactors (n: int) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable n = n
    try
        if n < 1 then
            printfn "%s" (("\nFactors of " + (string (n))) + " not computed")
            __ret <- ()
            raise Return
        printfn "%s" (("\nFactors of " + (string (n))) + ": ")
        let mutable fs: int array = [|1|]
        let rec apf (p: int) (e: int) =
            let mutable __ret = ()
            let mutable p = p
            let mutable e = e
            try
                let mutable orig: int = Seq.length (fs)
                let mutable pp: int = p
                let mutable i: int = 0
                while i < e do
                    let mutable j: int = 0
                    while j < orig do
                        fs <- Array.append fs [|(_idx fs (j)) * pp|]
                        j <- j + 1
                    i <- i + 1
                    pp <- pp * p
                __ret
            with
                | Return -> __ret
        let mutable e: int = 0
        let mutable m: int = n
        while (((m % 2 + 2) % 2)) = 0 do
            m <- int (m / 2)
            e <- e + 1
        apf (2) (e)
        let mutable d: int = 3
        while m > 1 do
            if (d * d) > m then
                d <- m
            e <- 0
            while (((m % d + d) % d)) = 0 do
                m <- int (m / d)
                e <- e + 1
            if e > 0 then
                apf (d) (e)
            d <- d + 2
        printfn "%s" (("[" + (unbox<string> (String.concat (" ") (Array.toList (Array.map string (fs)))))) + "]")
        printfn "%s" ("Number of factors = " + (string (Seq.length (fs))))
        __ret
    with
        | Return -> __ret
printFactors (-1)
printFactors (0)
printFactors (1)
printFactors (2)
printFactors (3)
printFactors (53)
printFactors (45)
printFactors (64)
printFactors (int (int 600851475143L))
printFactors (int (int 999999999999999989L))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
