// Generated 2025-08-06 20:48 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec binary_or (a: int) (b: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable a = a
    let mutable b = b
    try
        if (a < 0) || (b < 0) then
            __ret <- "ValueError"
            raise Return
        let mutable res: string = ""
        let mutable x: int = a
        let mutable y: int = b
        while (x > 0) || (y > 0) do
            let bit_a: int = ((x % 2 + 2) % 2)
            let bit_b: int = ((y % 2 + 2) % 2)
            if (bit_a = 1) || (bit_b = 1) then
                res <- "1" + res
            else
                res <- "0" + res
            x <- x / 2
            y <- y / 2
        if res = "" then
            res <- "0"
        __ret <- "0b" + res
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (binary_or (25) (32))
printfn "%s" (binary_or (37) (50))
printfn "%s" (binary_or (21) (30))
printfn "%s" (binary_or (58) (73))
printfn "%s" (binary_or (0) (255))
printfn "%s" (binary_or (0) (256))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
