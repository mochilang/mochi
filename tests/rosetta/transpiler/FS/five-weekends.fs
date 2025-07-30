// Generated 2025-07-30 21:05 +0700

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
let rec weekday (y: int) (m: int) (d: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable y = y
    let mutable m = m
    let mutable d = d
    try
        let mutable yy: int = y
        let mutable mm: int = m
        if mm < 3 then
            mm <- mm + 12
            yy <- yy - 1
        let k: int = ((yy % 100 + 100) % 100)
        let j: int = int (yy / 100)
        let a: int = int ((13 * (mm + 1)) / 5)
        let b: int = int (k / 4)
        let c: int = int (j / 4)
        __ret <- (((((((d + a) + k) + b) + c) + (5 * j)) % 7 + 7) % 7)
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let months31: int array = [|1; 3; 5; 7; 8; 10; 12|]
        let names: string array = [|"January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"|]
        let mutable count: int = 0
        let mutable firstY: int = 0
        let mutable firstM: int = 0
        let mutable lastY: int = 0
        let mutable lastM: int = 0
        let mutable haveNone: int array = [||]
        printfn "%s" "Months with five weekends:"
        for year in 1900 .. (2101 - 1) do
            let mutable hasOne: bool = false
            for m in months31 do
                if (int (weekday year m 1)) = 6 then
                    printfn "%s" ((("  " + (string year)) + " ") + (names.[m - 1]))
                    count <- count + 1
                    hasOne <- true
                    lastY <- year
                    lastM <- m
                    if firstY = 0 then
                        firstY <- year
                        firstM <- m
            if not hasOne then
                haveNone <- Array.append haveNone [|year|]
        printfn "%s" ((string count) + " total")
        printfn "%s" ""
        printfn "%s" "First five dates of weekends:"
        for i in 0 .. (5 - 1) do
            let day: int = 1 + (7 * i)
            printfn "%s" ((((("  Friday, " + (names.[firstM - 1])) + " ") + (string day)) + ", ") + (string firstY))
        printfn "%s" "Last five dates of weekends:"
        for i in 0 .. (5 - 1) do
            let day: int = 1 + (7 * i)
            printfn "%s" ((((("  Friday, " + (names.[lastM - 1])) + " ") + (string day)) + ", ") + (string lastY))
        printfn "%s" ""
        printfn "%s" "Years with no months with five weekends:"
        for y in haveNone do
            printfn "%s" ("  " + (string y))
        printfn "%s" ((string (Seq.length haveNone)) + " total")
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
