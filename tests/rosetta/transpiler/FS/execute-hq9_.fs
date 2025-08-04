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
let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

open System

let rec bottles (n: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable n = n
    try
        if n = 0 then
            __ret <- "No more bottles"
            raise Return
        if n = 1 then
            __ret <- "1 bottle"
            raise Return
        __ret <- (string (n)) + " bottles"
        raise Return
        __ret
    with
        | Return -> __ret
and sing99 () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable i: int = 99
        while i > 0 do
            printfn "%s" ((bottles (i)) + " of beer on the wall")
            printfn "%s" ((bottles (i)) + " of beer")
            printfn "%s" ("Take one down, pass it around")
            printfn "%s" ((bottles (i - 1)) + " of beer on the wall")
            i <- i - 1
        __ret
    with
        | Return -> __ret
and run (code: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable code = code
    try
        let mutable acc: int = 0
        let mutable i: int = 0
        while i < (String.length (code)) do
            let ch: string = _substring code i (i + 1)
            if ch = "H" then
                printfn "%s" ("Hello, World!")
            else
                if ch = "Q" then
                    printfn "%s" (code)
                else
                    if ch = "9" then
                        sing99()
                    else
                        if ch = "+" then
                            acc <- acc + 1
            i <- i + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let code: string = System.Console.ReadLine()
        run (code)
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
