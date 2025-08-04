// Generated 2025-08-04 15:16 +0700

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
let mutable bazCall: int = 0
let rec baz () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        bazCall <- bazCall + 1
        printfn "%s" ("baz: start")
        if bazCall = 1 then
            printfn "%s" ("baz: raising U0")
            __ret <- "U0"
            raise Return
        if bazCall = 2 then
            printfn "%s" ("baz: raising U1")
            __ret <- "U1"
            raise Return
        printfn "%s" ("baz: end")
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and bar () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        printfn "%s" ("bar: start")
        let mutable err: string = baz()
        if (String.length (err)) > 0 then
            __ret <- err
            raise Return
        printfn "%s" ("bar: end")
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and foo () =
    let mutable __ret : string = Unchecked.defaultof<string>
    try
        printfn "%s" ("foo: start")
        let mutable err: string = bar()
        if err = "U0" then
            printfn "%s" ("foo: caught U0")
        else
            if (String.length (err)) > 0 then
                __ret <- err
                raise Return
        err <- bar()
        if err = "U0" then
            printfn "%s" ("foo: caught U0")
        else
            if (String.length (err)) > 0 then
                __ret <- err
                raise Return
        printfn "%s" ("foo: end")
        __ret <- ""
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" ("main: start")
        let mutable err: string = foo()
        if (String.length (err)) > 0 then
            printfn "%s" ("main: unhandled " + err)
        else
            printfn "%s" ("main: success")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
